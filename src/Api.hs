{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Data.Int                     ( Int64 )
import Database.Persist.Postgresql  ( get
                                    , insert
                                    , delete
                                    , selectList
                                    , selectFirst
                                    , fromSqlKey
                                    , toSqlKey
                                    , Entity(..)
                                    , (==.)
                                    )
import Network.Wai                  ( Application )
import Servant
import Config                       ( Config(..) )
import Models


type API =
       "quizzes"
         :> Get '[JSON] [Quiz]
  :<|> "quizzes"
         :> Capture "id" QuizId
         :> Get '[JSON] Quiz
  :<|> "quizzes"
         :> ReqBody '[JSON] Quiz
         :> Post '[JSON] Quiz
  :<|> "quizzes"
         :> Capture "id" QuizId
         :> Delete '[JSON] ()
  :<|> "quizzes"
         :> Capture "quizId" QuizId
         :> ( "quizlets" :> Get '[JSON] [Quizlet] )
  :<|> "quizlets"
         :> Capture "id" QuizletId
         :> Get '[JSON] Quizlet
  :<|> "quizlets"
         :> ReqBody '[JSON] Quizlet
         :> Post '[JSON] Int64

-- This is not available in servant-0.5 so define it ourselves:
type Handler = ExceptT ServantErr IO

-- The context in which our API actions will run:
type AppM = ReaderT Config Handler

api :: Proxy API
api = Proxy

app :: Config -> Application
app config = serve api (readerServer config)

readerServer :: Config -> Server API
readerServer config = enter (readerToHandler config) readerServerT

-- Allow the injecting of Config while still returning Handler:
readerToHandler :: Config -> AppM :~> Handler
readerToHandler config = Nat $ \x -> runReaderT x config

readerServerT :: ServerT API AppM
readerServerT = listQuizzes
           :<|> getQuiz
           :<|> createQuiz
           :<|> deleteQuiz
           :<|> listQuizlets
           :<|> getQuizlet
           :<|> createQuizlet


-- Quiz API:

listQuizzes :: ReaderT Config Handler [Quiz]
listQuizzes = do
    storedQuizzes <- runDb (selectList [] [])
    let quizzes = map quizFromDb storedQuizzes
    return quizzes

getQuiz :: StoredQuizId -> AppM Quiz
getQuiz quizId = do
    maybeStoredQuiz <- runDb (selectFirst [StoredQuizId ==. quizId] [])
    let maybeQuiz = fmap quizFromDb maybeStoredQuiz
    case maybeQuiz of
         Nothing -> throwError err404
         Just quiz -> return quiz

createQuiz :: Quiz -> AppM Quiz
createQuiz quiz = do
    newQuizId <- runDb (insert (StoredQuiz (quizName quiz) (quizDescription quiz)))
    maybeStoredQuiz <- runDb (selectFirst [StoredQuizId ==. newQuizId] [])
    let maybeQuiz = fmap quizFromDb maybeStoredQuiz
    case maybeQuiz of
         Nothing -> throwError err404
         Just quiz -> return quiz

deleteQuiz :: StoredQuizId -> AppM ()
deleteQuiz quizId = do
  runDb (delete quizId)
  return ()

-- Quizlet API:

listQuizlets :: StoredQuizId -> AppM [Quizlet]
listQuizlets quizId = do
  storedQuizlets <- runDb (selectList [StoredQuizletStoredQuizId ==. quizId] [])
  let quizlets = map quizletFromDb storedQuizlets
  return quizlets

getQuizlet :: QuizletId -> AppM Quizlet
getQuizlet quizletId = do
  maybeStoredQuizlet <- runDb (selectFirst [StoredQuizletId ==. quizletId] [])
  let maybeQuizlet = fmap quizletFromDb maybeStoredQuizlet
  case maybeQuizlet of
    Nothing -> throwError err404
    Just quizlet -> return quizlet

createQuizlet :: Quizlet -> AppM Int64
createQuizlet quizlet = do
  let q = StoredQuizlet (toSqlKey $ quizletQuizId quizlet)
                        (quizletQuestion quizlet)
                        (quizletAnswer quizlet)
  newQuizlet <- runDb (insert q)
  return $ fromSqlKey newQuizlet
