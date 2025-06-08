namespace VoteAnalyzer

open System
open System.IO
open System.Net
open System.Net.Http
open System.Security.Cryptography
open System.Text
open System.Web
open System.Xml
open HtmlAgilityPack

type Downloader(hostName: string, cookie: string, leagueNameFilter: string, noCache: bool) =
    let selectElements (xpath: string) (node: XmlNode) =
        node.SelectNodes xpath
        |> Seq.cast<XmlElement>
        |> Seq.toList

    let selectSingleElement (xpath: string) (node: XmlNode) =
        node.SelectSingleNode xpath :?> XmlElement

    let selectElementText (xpath: string) (node: XmlNode) =
        node.SelectSingleNode xpath :?> XmlElement |> fun n -> n.InnerText.Trim() |> HttpUtility.HtmlDecode

    let elementExists (xpath: string) (node: XmlNode) =
        node.SelectSingleNode xpath :?> XmlElement |> fun n -> n <> null

    let selectAttributeValue (xpath: string) (node: XmlNode) =
        (node.SelectSingleNode xpath :?> XmlAttribute).Value

    let htmlFixes = [
        "::", "_"
        "x-on:", "x-on_"
        ":class", "_class"
        ":src", "_src"
        ":disabled", "_disabled"
        ":value", "_value"
    ]

    let userLeft = "[Left the"
    let resultsSongNamePath = "(.//a)[2]"
    let resultsPostedByPath = "(.//h6)[3]"
    let voteFromPath = ".//b"
    let votePointsPath = ".//h6"
    let baseUrl = $"https://{hostName}/"

    member _.Download() =
        use httpClientHandler = new HttpClientHandler(AllowAutoRedirect = false, UseCookies = true)
        httpClientHandler.CookieContainer.Add(Uri(baseUrl), Cookie(hostName, cookie))
        use httpClient = new HttpClient(httpClientHandler)

        let cacheDirectory = Path.Combine(Path.GetTempPath(), "_VoteCache")
        Console.WriteLine $"Cache directory: {cacheDirectory}"
        if not (Directory.Exists cacheDirectory) then
            Directory.CreateDirectory cacheDirectory |> ignore

        let loadXml(localPath: string) : XmlDocument =
            let calculateHash (url: Uri) =
                use md5 = MD5.Create()
                url.ToString()
                |> Encoding.UTF8.GetBytes
                |> md5.ComputeHash
                |> Array.map (fun b -> b.ToString "x2")
                |> String.concat ""

            let url = Uri(baseUrl + localPath.TrimStart '/')
            Console.WriteLine $"Loading {url}"

            let cachePath = Path.Combine(cacheDirectory, url |> calculateHash)

            use contentStream =
                match not noCache && File.Exists cachePath with
                | true -> File.OpenRead cachePath :> Stream
                | false ->
                    let replace (oldValue:string) (newValue:string) (input:string) = input.Replace(oldValue, newValue)
                    let enclose input prepend append = prepend + input + append

                    let doc = new HtmlDocument()
                    doc.OptionOutputAsXml <- true

                    htmlFixes
                    |> Seq.fold (fun acc (oldValue, newValue) -> acc |> replace oldValue newValue) (httpClient.GetStringAsync url).Result
                    |> enclose "<x>" "</x>"
                    |> doc.LoadHtml

                    let contentStream = new MemoryStream()
                    doc.Save contentStream

                    contentStream.Seek(0L, SeekOrigin.Begin) |> ignore

                    use fileStream = File.Create cachePath
                    contentStream.CopyTo fileStream

                    contentStream.Seek(0L, SeekOrigin.Begin) |> ignore
                    contentStream

            let xmlDoc = new XmlDocument()
            xmlDoc.Load contentStream
            xmlDoc


        loadXml "completed/-/completedLeagues"
        |> selectElements "//a"
        |> Seq.map (fun l -> l.GetAttribute "href", l.InnerText.Trim() |> HttpUtility.HtmlDecode)
        |> Seq.filter (fun (_, n) -> String.IsNullOrEmpty leagueNameFilter || n.Contains(leagueNameFilter, StringComparison.OrdinalIgnoreCase))
        |> Seq.map (fun (roundUrl, leagueName) ->

            loadXml $"{roundUrl}-/rounds"
            |> selectElements "//div[@class='card-body']"
            |> Seq.map (fun roundXml ->
                let roundName = roundXml |> selectElementText ".//div[@class='col']/h5"
                let roundLink = roundXml |> selectAttributeValue ".//a[contains(., 'RESULTS')]/@href"

                loadXml $"{roundLink}-/results"
                |> selectElements "//div[starts-with(@id, 'spotify:')]"
                |> Seq.filter (fun s -> not ((s |> selectElementText resultsPostedByPath).StartsWith userLeft))
                |> Seq.mapi (fun index songXml ->
                    let songId = songXml |> selectAttributeValue "@id"

                    let trackNode = (songXml |> (selectSingleElement resultsSongNamePath)).ParentNode.ParentNode
                    let songName = songXml |> selectElementText resultsSongNamePath
                    let artistName = trackNode |> selectElementText "(.//p)[1]"
                    let albumName = trackNode |> selectElementText "(.//p)[2]"
                    let songLink = $"https://open.spotify.com/track/{songId.Split(':')[2]}"
                    let postedBy = songXml |> selectElementText resultsPostedByPath

                    (
                        SongInfo(leagueName, roundName, songName, artistName, albumName, postedBy, songLink, index + 1, songId, roundLink, 0.0),

                        songXml
                        |> selectSingleElement ".//div[starts-with(@id, 'votes-')]"
                        |> selectElements "./div"
                        |> Seq.filter (fun s -> s |> selectElementText voteFromPath <> userLeft)
                        |> Seq.filter (fun s -> s |> elementExists votePointsPath)
                        |> Seq.map (fun voteXml ->
                            let from = voteXml |> selectElementText voteFromPath
                            let points = voteXml |> selectSingleElement votePointsPath |> fun n -> n.InnerText.Trim() |> HttpUtility.HtmlDecode |> int
                            VoteInfo(roundLink, songId, from, postedBy, points)
                        )
                    )
                )
            )
            |> Seq.concat
        )
        |> Seq.concat
        |> Seq.fold (fun (accSongs, accVotes) (song, votes) -> (song :: accSongs, Seq.append votes accVotes) ) ([], Seq.empty<VoteInfo>)
        |> fun (s, v) -> (s |> List.rev, v |> Seq.toList)
