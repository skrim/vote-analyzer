namespace VoteAnalyzer

type VoteInfo =
    struct
        val RoundId: string
        val SongId: string
        val From: string
        val To: string
        val Points: int

        new(roundId: string, songId: string, voteFrom: string, voteTo: string, points: int) =
            { RoundId = roundId; SongId = songId; From = voteFrom; To = voteTo; Points = points }
    end
