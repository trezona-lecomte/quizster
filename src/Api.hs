{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Control.Monad.Trans.Either   ( EitherT )
import Data.Int                     ( Int64 )
import Network.Wai                  ( Application )
import Database.Persist.Postgresql  ( insert, selectList, Entity(..)
                                    , fromSqlKey, (==.), selectFirst
                                    )
import Servant

import Config ( Config(..) )
import Models

-- Quiz API:

type QuizAPI =
       "quizzes" :> Get '[JSON] [Quiz]
  :<|> "quizzes" :> Capture "name" QuizId :> Get '[JSON] Quiz
  :<|> "quizzes" :> ReqBody '[JSON] Quiz :> Post '[JSON] Int64

quizAPI :: Proxy QuizAPI
quizAPI = Proxy

listQuizzes :: AppM [Quiz]
listQuizzes = do
    storedQuizzes <- runDb (selectList [] [])
    let quizzes = map entityVal storedQuizzes
    return quizzes

getQuiz :: QuizId -> AppM Quiz
getQuiz quizId = do
    maybeStoredQuiz <- runDb (selectFirst [QuizId ==. quizId] [])
    let maybeQuiz = fmap entityVal maybeStoredQuiz
    case maybeQuiz of
         Nothing -> throwError err404
         Just quiz -> return quiz

createQuiz :: Quiz -> AppM Int64
createQuiz quiz = do
    newQuiz <- runDb (insert (Quiz (quizName quiz) (quizDescription quiz)))
    return $ fromSqlKey newQuiz

-- Quizlet API:

type QuizletAPI =
       "quizzes"
         :> Capture "quizId" Int
         :> ( "quizlets"
                :> Get '[JSON] [Quizlet]
            )
  :<|> "quizlets"
         :> Capture "id" Int
         :> Get '[JSON] Quizlet
  :<|> "quizlets"
         :> ReqBody '[JSON] Quizlet
         :> Post '[JSON] Int

quizletAPI :: Proxy QuizletAPI
quizletAPI = Proxy

listQuizlets :: QuizId -> AppM [Quizlet]
listQuizlets quizId = do
  storedQuizlets <- runDb (selectList [QuizletQuizId ==. quizId] [])
  let quizlets = map entityVal storedQuizlets
  return quizlets

getQuizlet quizletId = do
  maybeStoredQuizlet <- runDb (selectFirst [QuizletId ==. quizletId] [])
  let maybeQuizlet = fmap entityVal maybeStoredQuizlet
  case maybeQuizlet of
    Nothing -> throwError err404
    Just quizlet -> return quizlet

createQuizlet quizlet = do
  let q = Quizlet (quizletQuizId quizlet)
                  (quizletQuestion quizlet)
                  (quizletAnswer quizlet)
  newQuizlet <- runDb (insert q)
  return $ fromSqlKey newQuizlet

-- Wiring:

-- newtype App a
--     = App
--     { runApp :: ReaderT Config (ExceptT ServantErr IO) a
--     } deriving ( Functor, Applicative, Monad, MonadReader Config,
--                  MonadError ServantErr, MonadIO)

type API = "quizzes"  :> QuizAPI
      :<|> "quizlets" :> QuizletAPI

-- app :: Config -> Application
-- app cfg = serve api (readerServer cfg)

-- readerServer :: Config -> Server API
-- readerServer cfg = enter (convertApp cfg) server

-- convertApp :: Config -> App :~> ExceptT ServantErr IO
-- convertApp cfg = Nat (flip runReaderT cfg . runApp)

--

type AppM = ReaderT Config (EitherT ServantErr IO)

api :: Proxy API
api = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither config = Nat $ \x -> runReaderT x config

readerServer :: Config -> Server API
readerServer config = enter (readerToEither config) server

app :: Config -> Application
app config = server api (readerServer config)

-- configuredServer :: Config ->

-- server :: Server API
-- server = enter configuredServer configuredServerT

-- app :: Application
-- app = serve api server
