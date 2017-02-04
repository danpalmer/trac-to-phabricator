
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}


module Lib
    ( migrate
    , describeTicket
    , WorkDescription(.., DownTo)
    ) where

import qualified Data.Text as T
import Data.List (find, (\\))

import Trac
import Phabricator
import Config
import Types
import Debug.Trace
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative

import qualified Database.MySQL.Base as M
import qualified Database.PostgreSQL.Simple as P



migrate :: WorkDescription -> IO ()
migrate workDesc = do
    tracConn <- connectTrac
    pc@PC{..} <- connectPhab

    -- Users
    phabricatorUsers <- getPhabricatorUsers pcUser
    tracUsers <- getTracUsers tracConn
    traceShowM ("phabUsers", length phabricatorUsers)
    traceShowM ("tracUsers", length tracUsers)

    projectMap <- getProjectMap tracConn pcProject
    traceShowM ("words", projectMap)

    deleteTicketInfo pcManiphest
    tracTickets <- getTracTickets tracConn
    traceShowM ("tickets", length tracTickets)
--    let tracTickets' = getTickets workDesc (sortBy (comparing t_id) tracTickets)
--    let good = t_id <$> filter (not . null . t_cc) tracTickets'
--    traceShowM good
--    traceShowM tracTickets'
--    traceShowM tracTickets'
    let phabricatorTickets =
           map (tracTicketToPhabricatorTicket phabricatorUsers projectMap)
              tracTickets
--    traceShowM phabricatorTickets
    createPhabricatorTickets pcManiphest workDesc phabricatorTickets

    P.close tracConn
    closePhab pc
--    putStrLn $ "Migrated " ++ show (length tracTickets') ++ " tickets."


getProjectMap :: P.Connection -> C 'Project -> IO ProjectMap
getProjectMap tracConn pcProject = do
    --deleteProjectInfo pcProject
    -- Projects
    {-
    ps@Projects {..} <- getProjectWords tracConn
    traceShowM ps
    createProjectHierarchy "GHC" Mil milestones
    createProjectHierarchy "Component" Sub comp
    createProjectHierarchy "OS" Sub oses
    createProjectHierarchy "Architecture" Sub arch

    -- Make tags for keywords
    mapM_ createProject keywords
    mapM_ createProject types
    -}
    getPhabricatorProjects pcProject


createProjectHierarchy :: T.Text -> SubOrMil -> [T.Text] -> IO ()
createProjectHierarchy parent sorm children = do
  Just newProj <- createProject parent
  mapM_ (addSubproject newProj sorm) children


type ProjectMap = [PhabricatorProject]
type UserMap    = [PhabricatorUser]




describeTicket :: TracTicket -> String
describeTicket ticket = T.unpack $
    T.concat [
        t_summary ticket,
            "\n\tFields: ", textLength $ t_customFields ticket,
            "\n\tComments:", textLength $ t_changes ticket,
            "\n\n"
    ]
    where textLength = T.pack . show . length


tracTicketToPhabricatorTicket :: [PhabricatorUser] -> [PhabricatorProject] ->
                                 TracTicket -> ManiphestTicket
tracTicketToPhabricatorTicket users projects ticket = do
    ManiphestTicket
        { m_tracn = t_id ticket
        , m_title = t_summary ticket
        , m_description = convert (t_id ticket) (readCommentMap (t_id ticket))
                            <$> t_description ticket
        , m_ownerPHID = findUser users <$> t_owner ticket
        , m_authorPHID = findUser users $ t_reporter ticket
        , m_priority = convertPriority $ t_priority ticket
        , m_created = t_time ticket
        , m_modified = t_changetime ticket
        , m_status = t_status ticket
        , m_changes = otherTicketChanges
        , m_cc = mapMaybe lookupCC (t_cc ticket)
        , m_projects = toProject (t_milestone ticket)
                        ++ toProject (Just $ t_type ticket)
                        ++ toProject (Just $ t_component ticket)
                        ++ toProject (t_os ticket)
                        ++ toProject (t_architecture ticket)
                        ++ (mapMaybe lkupProj (t_keywords ticket))
        , m_commits = map (convertCommit users) (t_commits ticket)
        , m_attachments = map (convertAttachment users) (t_attachments ticket)
        -- These are all changes we use editdependencies for
        , m_diffs = t_diffs ticket
        , m_diffHistory = diffChanges
        , m_blocking = t_blocking ticket
        , m_blockedby = t_blockedby ticket
        , m_related = t_related ticket
        }
    where
          ticketChanges =
            concatMap (tracChangeToPhabChange (t_id ticket) users projects)
                      (t_changes ticket)

          (diffChanges, otherTicketChanges)
            = partition isDiffChange ticketChanges

          isDiffChange (ManiphestChange { mc_type = t}) =
            case t of
              MCDifferential {} -> True
              MCRelated {} -> True
              MCBlockedBy {} -> True
              MCBlocking {} -> True
              _ -> False
          -- CC field is either an email or a username
          lookupCC t = lookupPhabricatorUserPHID users t <|> lookupByEmail t

          lkupProj = lookupPhabricatorProjectPHID projects

          toProject v = maybeToList (lkupProj =<< v)

findUser :: UserMap -> T.Text -> UserID
findUser users u = fromMaybe botUser (lookupPhabricatorUserPHID users u)

convertAttachment :: UserMap -> Attachment -> ManiphestAttachment
convertAttachment users Attachment{..} =
  ManiphestAttachment
    { ma_tracn = a_id
    , ma_name  = a_name
--    , ma_size  = a_size
    , ma_time  = a_time
    , ma_desc  = a_desc
    , ma_author = findUser users (a_author) }

convertCommit :: UserMap -> TCommit -> ManiphestCommit
convertCommit _ TCommit{..} =
  ManiphestCommit
    { mc_id = c_id
    , mc_author = botUser
    , mc_time  = c_time }


-- This ticket causes the parser to hang because of an insane table
-- Need to import it and then edit the markup manually
badTicket :: Int
badTicket = 8539

-- I don't have mails to check now
lookupByEmail :: T.Text -> Maybe UserID
lookupByEmail = const Nothing

tracChangeToPhabChange :: Int -> [PhabricatorUser] -> [PhabricatorProject]
                       -> TracTicketChange -> [ManiphestChange]
tracChangeToPhabChange n users projects TracTicketChange{..}
  = map (\t -> ManiphestChange
                { mc_type    = t
                , mc_created = ch_time
                , mc_authorId = findUser ch_author
                , mc_commentn = ch_commentn }) (getType ch_field)
  where
    findUser u = fromMaybe botUser (lookupPhabricatorUserPHID users u)
    getType :: T.Text -> [MCType]
    getType t =
      case t of
        "comment" ->
          [MCComment 0 . (if n == badTicket then id
                                            else convert n (readCommentMap n))
                     $ fromMaybe "" ch_newvalue]
        "cc"      -> ccs
        "architecture" -> addRemoveKeywords --maybe Dummy MCArchitecture ch_newvalue
        "blockedby" -> blockedby
        "blocking"  -> blocking
        "component" -> addRemoveKeywords --m MCComponent
        "description" -> [Dummy "MCDesc"] -- [MCDescription (convert $ fromMaybe "" ch_newvalue)]
        "differential" -> diffs
        "difficulty"   -> [Dummy "Difficulty"] -- m MCDifficulty
        "failure"      -> addRemoveKeywords --m MCFailure
        "keywords"     -> keywords
        "milestone"    -> addRemoveKeywords -- m MCMilestone
        "os"           -> addRemoveKeywords --m MCOS
        "owner"        -> m (MCOwner . lookupPhabricatorUserPHID users)
        "patch"        -> [MCPatch]
        "priority"     -> [MCPriority (maybe Normal convertPriority ch_newvalue)]
        "related"      -> related
        "reporter"     -> [MCReporter]
        "resolution"   -> m (\s -> if T.null s then Dummy "Unset resolution"
                                               else MCResolution s)
                       -- When a resolution is unset, the next event is
                       -- a ticket reopen
        "severity"     -> [MCSeverity]
        "status"       -> [case ch_newvalue of
                            Nothing -> Dummy "null field"
                            Just v -> if v == "closed" then Dummy "closed" else MCStatus v] -- "A closed ticket always has a resolution" -- Mao Zedong
        "summary"      -> m MCSummary
        "testcase"     -> [MCTestcase]
        "type"         -> addRemoveKeywords --m MCType
        "version"      -> [Dummy "VERSION"] -- m MCVersion
        "wikipage"     -> m MCWiki
        s         -> [Dummy s]
        -- Note there are lots of entries like _comment1 which correspond
        -- to comment updates. However, the value in comment is the actual
        -- final comment and we don't both to maintain this much fidelity.
        --
    -- used by anything which gets mapped to a project
    keywords = [MCRemoveKeyword toRemove, MCAddKeyword toAdd]
      where
        newWords = maybe [] convertKeywords ch_newvalue
        oldWords = maybe [] convertKeywords ch_oldvalue

        toAdd = newWords \\ oldWords
        toRemove = oldWords \\ newWords
    -- Mentions add these edges so we can't just set
    ccs = [MCCCRemove toRemove, MCCCAdd toAdd]
      where
        newWords = maybe [] convertCC ch_newvalue
        oldWords = maybe [] convertCC ch_oldvalue

        toAdd = newWords \\ oldWords
        toRemove = oldWords \\ newWords
    -- Mentioning also adds these kinds of edges so we can't just SET
    diffs = addRemoveField MCDifferential parseDiff
    -- Needs to be like this because blocking also adds these kinds of
    -- edges
    related = addRemoveField MCRelated parseTicketName
    blockedby = addRemoveField MCBlockedBy parseTList
    blocking = addRemoveField MCBlocking parseTList

    addRemoveField con parser = [con toRemove toAdd]
      where
        newWords = parser ch_newvalue
        oldWords = parser ch_oldvalue

        toAdd = newWords \\ oldWords
        toRemove = oldWords \\ newWords






    addRemoveKeywords = fromMaybe ([Dummy "null field"]) $ do
      a <- milestoneRenaming <$> ch_oldvalue
      b <- ch_newvalue
      -- Fail if the new project isn't found. This might be dodgy..
      newProject <- lookupPhabricatorProjectPHID projects (milestoneRenaming b)
      let oldProject = lookupPhabricatorProjectPHID projects a
      return $ if Just newProject == oldProject
          then []
          else catMaybes [MCRemoveKeyword . (:[]) <$> oldProject]
                ++ [MCAddKeyword [newProject]]

    m con = [maybe (Dummy ("null field:")) con ch_newvalue]

    lookupCC t = lookupPhabricatorUserPHID users t <|> lookupByEmail t

    convertCC :: T.Text -> [UserID]
    convertCC t = mapMaybe lookupCC (parse_cc t)

    convertKeywords :: T.Text -> [ProjectID]
    convertKeywords t = mapMaybe (lookupPhabricatorProjectPHID projects) (parse_cc t)


convertPriority :: T.Text -> ManiphestPriority
convertPriority priority =
    case priority of
        "highest" -> Unbreak
        "high" -> High
        "normal" -> Normal
        "low" -> Low
        "lowest" -> Wishlist
        _ -> Triage


lookupPhabricatorUserPHID :: [PhabricatorUser] -> T.Text -> Maybe UserID
lookupPhabricatorUserPHID users username  =
    u_phid <$> find (\x -> u_userName x == username) users

milestoneRenaming :: T.Text -> T.Text
milestoneRenaming "7.12.1" = "8.0.1"
milestoneRenaming "_|_" = "⊥"
milestoneRenaming s = s

lookupPhabricatorProjectPHID :: [PhabricatorProject] -> T.Text -> Maybe ProjectID
lookupPhabricatorProjectPHID _ "" = Nothing
lookupPhabricatorProjectPHID projects (milestoneRenaming -> project)  =
  case (p_phid <$> find (\x -> p_projectName x == project) projects) of
    Just v -> Just v
    Nothing -> traceShow ("Not Found", project) Nothing


