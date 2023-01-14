module LeaderElection where

-- Implementation of a leader election algorithm
-- Based on Raft
-- https://web.stanford.edu/~ouster/cgi-bin/cs190-winter20/lecture.php?topic=raft
-- https://en.wikipedia.org/wiki/Raft_(algorithm)

data ServerState = Leader | Follower | Candidate deriving (Show, Eq)

data Event = HeartbeatTimedOut | ReceivedMajorityVote | DiscoveredNewTerm deriving (Show, Eq)

data Action = StartLeaderProcess | StartFollowerProcess | NoOp deriving (Show, Eq)

data Config = Config
    { heartbeatTimeout :: Int
    }

type TransitionServerState = (ServerState, Event) -> (ServerState, Action)

startup :: ServerState
startup = Follower

transition :: TransitionServerState
transition stateWithEvent = case stateWithEvent of
    (Follower, HeartbeatTimedOut) -> (Candidate, undefined)
    (Candidate, HeartbeatTimedOut) -> (Candidate, undefined)
    (Candidate, ReceivedMajorityVote) -> (Leader, undefined)
    (Leader, DiscoveredNewTerm) -> (Follower, StartFollowerProcess)
    _ -> undefined

followerProcess :: IO ()
followerProcess = undefined

leaderProcess :: IO ()
leaderProcess = undefined
