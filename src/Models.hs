{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson                  ( ToJSON, FromJSON )
import GHC.Generics                ( Generic )
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Database.Persist.Sql
import Database.Persist.TH

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Quiz
    name String
    description String
    deriving Eq Show Generic
Quizlet
    quizId QuizId
    question String
    answer String
    deriving Eq Show Generic
|]

instance ToJSON Quiz
instance FromJSON Quiz
instance ToJSON Quizlet
instance FromJSON Quizlet

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
