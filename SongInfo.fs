namespace VoteAnalyzer

    type SongInfo =
        struct
            val LeagueName: string
            val RoundName: string
            val Position: int
            val PostedBy: string
            val SongLink: string
            val SongName: string
            val ArtistName: string
            val AlbumName: string
            val SongId : string
            val RoundId : string
            val PointsShare : float

            new(leagueName: string, roundName: string, songName: string, artistName: string, albumName: string, postedBy: string, songLink: string, position: int, songId : string, roundId: string, pointsShare: float) =
                { LeagueName = leagueName; RoundName = roundName; SongName = songName; ArtistName = artistName; AlbumName = albumName; PostedBy = postedBy; SongLink = songLink; Position = position; SongId = songId; RoundId = roundId; PointsShare = pointsShare }
        end
