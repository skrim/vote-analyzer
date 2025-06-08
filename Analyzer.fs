namespace VoteAnalyzer

open System
open System.Globalization
open System.IO
open CsvHelper

type Analyzer(songs: seq<SongInfo>, votes: seq<VoteInfo>) =

    let minRounds = 6

    let songIdsByRound =
        songs
        |> Seq.groupBy (fun s -> s.RoundId)
        |> Seq.map (fun (k, v) -> k, v |> Seq.map (fun s -> s.SongId) |> Set.ofSeq)
        |> Map.ofSeq

    let participants =
        songs
        |> Seq.map (fun s -> s.PostedBy)
        |> Seq.distinct
        |> Seq.sortBy (fun s -> s.ToLowerInvariant())

    let roundsOfParticipants =
        participants
        |> Seq.map (fun p ->
            let rounds = votes |> Seq.filter (fun s -> s.From = p) |> Seq.map (fun s -> s.RoundId) |> Set.ofSeq
            p, rounds
        )
        |> Map.ofSeq

    let playersWithEnoughRounds =
        participants
        |> Seq.filter (fun p ->
            roundsOfParticipants
            |> Map.find p
            |> Seq.length >= minRounds
        )

    let ownSongs =
        songs
        |> Seq.map (fun s -> s.PostedBy, s.RoundId, s.SongId)
        |> Set.ofSeq

    let votesOfPlayersByRound =
        votes
        |> Seq.groupBy (fun v -> v.RoundId)
        |> Seq.map (fun (roundId, votesOfRound) ->
            roundId,
            votesOfRound
            |> Seq.groupBy (fun v -> v.From)
            |> Seq.map (fun (player, votesOfPlayer) ->
                player,
                votesOfPlayer
                |> Seq.map (fun v -> v.SongId, v)
                |> Map.ofSeq)
            |> Map.ofSeq)
        |> Map.ofSeq

    let getVote player roundId songId =
        match votesOfPlayersByRound |> Map.find roundId |> Map.find player |> Map.tryFind songId with
        | Some v -> v.Points
        | None ->
            match ownSongs |> Set.contains (player, roundId, songId) with
            | true -> 5 // own song gets 5 points
            | false -> 0

    let writePlayerTable (fileName: string) (players: string seq) (values: float seq seq) =
        Console.WriteLine $"Writing {fileName}"
        use writer = new StreamWriter(fileName)
        use csv = new CsvWriter(writer, CultureInfo.InvariantCulture)

        let playerRoundcount player = roundsOfParticipants |> Map.find player |> Seq.length

        let writeRow first second rest =
            csv.WriteField(first |> string)
            csv.WriteField(second |> string)
            rest |> Seq.iter (fun v -> csv.WriteField(v |> string))
            csv.NextRecord()

        writeRow "" "" players
        writeRow "" "" (players |> Seq.map playerRoundcount)

        (players, values)
        ||> Seq.iter2 (fun player row ->
            writeRow player (playerRoundcount player) (row |> Seq.map (fun v -> if v = Double.MinValue then "" else v |> string))
        )

    member _.WriteSongs() =
        Console.WriteLine "Writing songs.csv";
        use writer = new StreamWriter "songs.csv"
        use csv = new CsvWriter(writer, CultureInfo.InvariantCulture)
        csv.WriteRecords songs

    member _.WriteVoteCosineSimilarity() =
        let dotProduct seq1 seq2 = Seq.map2 (fun a b -> a * b) seq1 seq2 |> Seq.sum
        let magnitude seq = seq |> Seq.map (fun x -> x * x) |> Seq.sum |> Math.Sqrt

        // mean of player votes is 0
        let centerVotes votes =
            let mean = votes |> Seq.average
            votes |> Seq.map (fun v -> v - mean)

        playersWithEnoughRounds
        |> Seq.map (fun player1 ->
            let player1Rounds = roundsOfParticipants |> Map.find player1

            playersWithEnoughRounds
            |> Seq.map (fun player2 ->

                let player2Rounds = roundsOfParticipants |> Map.find player2

                let commonRounds = Set.intersect player1Rounds player2Rounds

                match Set.count commonRounds with
                | c when c < minRounds -> Double.MinValue
                | _ when player1 = player2 -> Double.MinValue
                | _ ->
                    let votes1, votes2 =
                        commonRounds
                        |> Seq.collect (fun roundId ->
                            songIdsByRound
                            |> Map.find roundId
                            |> Seq.map (fun songId ->
                                let v1 = getVote player1 roundId songId |> float
                                let v2 = getVote player2 roundId songId |> float
                                v1, v2
                            )
                        )
                        |> Seq.toList
                        |> List.unzip

                    let centered1 = centerVotes votes1
                    let centered2 = centerVotes votes2

                    let dot = dotProduct centered1 centered2

                    match magnitude centered1, magnitude centered2 with
                    | 0.0, 0.0 -> 0.0
                    | m1, m2 -> dot / (m1 * m2)
            )
        )
        |> writePlayerTable "vote_cosine_similarity.csv" playersWithEnoughRounds

    member _.WritePlayerStatistics() =
        Console.WriteLine "Writing player_statistics.csv"

        let mean (values: float list) =
            if List.isEmpty values then Double.NaN else values |> List.average

        let median (values: float list) =
            match values |> List.sort with
            | [] -> Double.NaN
            | sorted ->
                let len = List.length sorted
                if len % 2 = 1 then
                    sorted |> List.item (len / 2)
                else
                    let mid = len / 2
                    (sorted.[mid - 1] + sorted.[mid]) / 2.0

        let standardDeviation (values: float list) =
            match values with
            | [] -> Double.NaN
            | _ ->
                let m = mean values
                values
                |> List.averageBy (fun v -> (v - m) ** 2.0)
                |> Math.Sqrt

        let skewness (values: float list) =
            match values with
            | [] -> Double.NaN
            | _ ->
                let m = mean values
                let sd = standardDeviation values
                if sd = 0.0 then 0.0
                else
                    values
                    |> List.averageBy (fun v -> ((v - m) / sd) ** 3.0)

        use writer = new StreamWriter("player_statistics.csv")
        use csv = new CsvWriter(writer, CultureInfo.InvariantCulture)

        csv.WriteField "PlayerName"
        csv.WriteField "Rounds"
        csv.WriteField "SongsInRounds"
        for i in 0 .. 5 do
            csv.WriteField($"Points_{i}")
        csv.WriteField "Mean"
        csv.WriteField "Median"
        csv.WriteField "StandardDeviation"
        csv.WriteField "Skewness"
        csv.NextRecord()

        for player in participants do
            let rounds = roundsOfParticipants |> Map.find player
            let songsInRounds =
                rounds
                |> Seq.sumBy (fun r -> songIdsByRound |> Map.find r |> Set.count)

            let votes =
                rounds
                |> Seq.collect (fun roundId ->
                    songIdsByRound
                    |> Map.find roundId
                    |> Seq.map (fun songId -> getVote player roundId songId |> float))
                |> Seq.toList

            let pointPercentages =
                if songsInRounds = 0 then
                    [0 .. 5] |> List.map (fun _ -> 0.0)
                else
                    [0 .. 5]
                    |> List.map (fun p ->
                        votes
                        |> List.filter (fun v -> int v = p)
                        |> List.length
                        |> fun c -> float c / float songsInRounds)

            let meanVal = mean votes
            let medianVal = median votes
            let sdVal = standardDeviation votes
            let skewVal = skewness votes

            csv.WriteField player
            csv.WriteField(Seq.length rounds |> string)
            csv.WriteField(songsInRounds |> string)
            pointPercentages |> List.iter (fun pct -> csv.WriteField(pct |> string))
            csv.WriteField(meanVal |> string)
            csv.WriteField(medianVal |> string)
            csv.WriteField(sdVal |> string)
            csv.WriteField(skewVal |> string)
            csv.NextRecord()
