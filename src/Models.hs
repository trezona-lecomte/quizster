{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson                  ( ToJSON, FromJSON, toJSON, parseJSON )
import GHC.Generics                ( Generic )
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Int                    ( Int64 )
import Database.Persist.Sql
import Database.Persist.TH

import Config


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredQuiz
    name String
    description String
    deriving Eq Show Generic
StoredQuizlet
    storedQuizId StoredQuizId
    question String
    answer String
    deriving Eq Show Generic
|]

type QuizId = StoredQuizId
type QuizletId = StoredQuizletId

data Quiz = Quiz { quizId :: Int64
                 , quizName :: String
                 , quizDescription :: String
                 } deriving (Eq, Show, Generic)

data Quizlet = Quizlet { quizletId :: Int64
                       , quizletQuizId :: Int64
                       , quizletQuestion :: String
                       , quizletAnswer :: String
                       } deriving (Eq, Show, Generic)

instance ToJSON Quiz
instance FromJSON Quiz
instance ToJSON Quizlet
instance FromJSON Quizlet

quizFromDb :: Entity StoredQuiz -> Quiz
quizFromDb entity =
  Quiz { quizId          = fromSqlKey            $ entityKey entity
       , quizName        = storedQuizName        $ entityVal entity
       , quizDescription = storedQuizDescription $ entityVal entity
       }

quizletFromDb :: Entity StoredQuizlet -> Quizlet
quizletFromDb entity =
  Quizlet { quizletId       = fromSqlKey                             $ entityKey entity
          , quizletQuizId   = fromSqlKey $ storedQuizletStoredQuizId $ entityVal entity
          , quizletQuestion = storedQuizletQuestion                  $ entityVal entity
          , quizletAnswer   = storedQuizletAnswer                    $ entityVal entity
          }

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
