open System.CommandLine
open System.Threading.Tasks
open VoteAnalyzer

[<EntryPoint>]
let main args =
    let hostNameArgument = Argument<string>("hostname", "The host name")
    let cookieArgument = Argument<string>("cookie", "The cookie to use for authentication")
    let filterOption = Option<string>("--filter", "Name name filter")
    let noCacheOption = Option<bool>("--no-cache", "Don't use cache (use if data has changed)")

    let analyzeCommand = Command("analyze", "Analyze the vote data")
    analyzeCommand.AddArgument hostNameArgument
    analyzeCommand.AddArgument cookieArgument
    analyzeCommand.AddOption filterOption
    analyzeCommand.AddOption noCacheOption

    analyzeCommand.SetHandler((fun hostName cookie filter noCache ->

        let downloader = Downloader(hostName, cookie, filter, noCache)
        let songs, votes = downloader.Download()

        let analyzer = Analyzer(songs, votes)
        analyzer.WriteSongs()
        analyzer.WriteVoteCosineSimilarity()

        Task.CompletedTask
    ), hostNameArgument, cookieArgument, filterOption, noCacheOption)

    analyzeCommand.InvokeAsync(args)
    |> Async.AwaitTask
    |> Async.RunSynchronously
