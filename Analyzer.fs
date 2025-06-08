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

        let pointsBySong =
            votes
            |> Seq.groupBy (fun v -> v.SongId)
            |> Seq.map (fun (k, vs) -> k, vs |> Seq.sumBy (fun v -> v.Points) |> float)
            |> Map.ofSeq

        let pointsByRound =
            votes
            |> Seq.groupBy (fun v -> v.RoundId)
            |> Seq.map (fun (k, vs) -> k, vs |> Seq.sumBy (fun v -> v.Points) |> float)
            |> Map.ofSeq

        let songsWithShare =
            songs
            |> Seq.map (fun s ->
                let songPoints = pointsBySong |> Map.tryFind s.SongId |> Option.defaultValue 0.0
                let roundPoints = pointsByRound |> Map.tryFind s.RoundId |> Option.defaultValue 0.0
                let share = if roundPoints = 0.0 then 0.0 else songPoints / roundPoints
                SongInfo(s.LeagueName, s.RoundName, s.SongName, s.ArtistName, s.AlbumName, s.PostedBy, s.SongLink, s.Position, s.SongId, s.RoundId, share)
            )

        use writer = new StreamWriter "songs.csv"
        use csv = new CsvWriter(writer, CultureInfo.InvariantCulture)
        csv.WriteRecords songsWithShare

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