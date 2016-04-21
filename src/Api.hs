{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Data.Int                     ( Int64 )
import Network.Wai                  ( Application )
import Database.Persist.Postgresql  ( insert, selectList, Entity(..)
                                    , fromSqlKey, (==.), selectFirst
                                    )
import Servant

import Config ( Config(..) )
import Models


type API =
       "quizzes"
         :> Get '[JSON] [Quiz]
  :<|> "quizzes"
         :> Capture "id" QuizId
         :> Get '[JSON] Quiz
  :<|> "quizzes"
         :> ReqBody '[JSON] Quiz
         :> Post '[JSON] Int64
  :<|> "quizzes"
         :> Capture "quizId" QuizId
         :> ( "quizlets" :> Get '[JSON] [Quizlet] )
  :<|> "quizlets"
         :> Capture "id" QuizletId
         :> Get '[JSON] Quizlet
  :<|> "quizlets"
         :> ReqBody '[JSON] Quizlet
         :> Post '[JSON] Int64

type AppM = ReaderT Config Handler

api :: Proxy API
api = Proxy

app :: Config -> Application
app config = serve api (readerServer config)

readerServer :: Config -> Server API
readerServer config = enter (readerToHandler config) readerServerT

readerToHandler :: Config -> AppM :~> Handler
readerToHandler config = Nat $ \x -> runReaderT x config

readerServerT :: ServerT API AppM
readerServerT = listQuizzes
           :<|> getQuiz
           :<|> createQuiz
           :<|> listQuizlets
           :<|> getQuizlet
           :<|> createQuizlet


-- Quiz API:

listQuizzes :: ReaderT Config Handler [Quiz]
listQuizzes = do
    storedQuizzes <- runDb (selectList [] [])
    let quizzes = map entityVal storedQuizzes
    return quizzes

getQuiz :: (MonadReader Config m, MonadIO m, MonadError ServantErr m) => QuizId -> m Quiz
getQuiz quizId = do
    maybeStoredQuiz <- runDb (selectFirst [QuizId ==. quizId] [])
    let maybeQuiz = fmap entityVal maybeStoredQuiz
    case maybeQuiz of
         Nothing -> throwError err404
         Just quiz -> return quiz

createQuiz :: (MonadReader Config m, MonadIO m) => Quiz -> m Int64
createQuiz quiz = do
    newQuiz <- runDb (insert (Quiz (quizName quiz) (quizDescription quiz)))
    return $ fromSqlKey newQuiz


-- Quizlet API:

listQuizlets :: (MonadReader Config m, MonadIO m) => QuizId -> m [Quizlet]
listQuizlets quizId = do
  storedQuizlets <- runDb (selectList [QuizletQuizId ==. quizId] [])
  let quizlets = map entityVal storedQuizlets
  return quizlets

getQuizlet :: (MonadReader Config m, MonadIO m, MonadError ServantErr m) => QuizletId -> m Quizlet
getQuizlet quizletId = do
  maybeStoredQuizlet <- runDb (selectFirst [QuizletId ==. quizletId] [])
  let maybeQuizlet = fmap entityVal maybeStoredQuizlet
  case maybeQuizlet of
    Nothing -> throwError err404
    Just quizlet -> return quizlet

createQuizlet :: (MonadReader Config m, MonadIO m) => Quizlet -> m Int64
createQuizlet quizlet = do
  let q = Quizlet (quizletQuizId quizlet)
                  (quizletQuestion quizlet)
                  (quizletAnswer quizlet)
  newQuizlet <- runDb (insert q)
  return $ fromSqlKey newQuizlet