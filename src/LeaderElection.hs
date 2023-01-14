module LeaderElection where

-- Implementation of a leader election algorithm
-- Based on Raft
-- https://web.stanford.edu/~ouster/cgi-bin/cs190-winter20/lecture.php?topic=raft
-- https://en.wikipedia.org/wiki/Raft_(algorithm)

data ServerState
    = Leader
    | Follower
    | Candidate
    deriving (Show, Eq)

data Event
    = HeartbeatTimedOut
    | ReceivedMajorityVote
    | DiscoveredNewTerm
    | DiscoveredCurrentLeader
    deriving (Show, Eq)

data Action
    = StartLeaderProcess
    | StartFollowerProcess
    | StartElection
    | NoOp
    deriving (Show, Eq)

data Config = Config
    { heartbeatTimeout :: Int
    }

type TransitionServerState = (ServerState, Event) -> (ServerState, Action)

startup :: ServerState
startup = Follower

transition :: TransitionServerState
transition stateWithEvent = case stateWithEvent of
    -- Leader might be dead since the heartbeat timedout. Start election.
    (Follower, HeartbeatTimedOut) -> (Candidate, StartElection)
    (Candidate, HeartbeatTimedOut) -> (Candidate, undefined)
    (Candidate, ReceivedMajorityVote) -> (Leader, undefined)
    (Candidate, DiscoveredNewTerm) -> (Follower, StartFollowerProcess)
    (Candidate, DiscoveredCurrentLeader) -> (Follower, StartFollowerProcess)
    (Leader, DiscoveredNewTerm) -> (Follower, StartFollowerProcess)
    _ -> undefined -- TODO indicate invalid transition

followerProcess :: IO ()
followerProcess = undefined

leaderProcess :: IO ()
leaderProcess = undefined

-- Vote for self
-- Increment current election term
startElection :: IO ()
startElection = undefined
