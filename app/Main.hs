{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import           System.Random
import System.Random(Random(randomRIO))
import           Control.Applicative              ((<|>))
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import System.IO

data Model = Model
  { anecText :: Text
  , todoItems :: [TodoItem]
  } deriving (Show)

type TodoItem = Text

addItem :: TodoItem -> Model -> Model
addItem item model = model
  { todoItems = item : todoItems model }

showItems :: [TodoItem] -> Text
showItems items = Text.unlines items

removeItem:: TodoItem -> Model -> Either Text Model
removeItem item model
  | item `notElem` items
      = Left ("No such item: " <> item)
  | otherwise = Right model
      { todoItems = filter (/= item) items }
  where
    items = todoItems model

removeItemByIx :: Int -> Model -> Model
removeItemByIx n model = model
  { todoItems = take (n - 1) items ++ drop n items }
    where
      items = todoItems model

data Action
  = DoNothing
  | AddItem Text
  | ShowItems
  | RemoveItem TodoItem
  | GetTime
  | Start
  | Anec 
  | Armyane
  | Shlyapa
  | Poruchic
  | Medved
  | Zashli
  | Raznoe
  | Randomniy
  deriving (Show)

-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = Text.unlines
 [ "Добрый день, вас приветствует высокофункциональный бот"
 , "Botskell!"
 , "Я владею более чем шестью миллионами форм общения! Ой,"
 , "перепутал..."
 , "В общем, я пока слабо развит, и могу лишь рассказывать анекдоты."
 , "Но я активно развиваюсь, поэтому вот какие функции планируются:"
 , ""
 , "Сделать напоминалку - поставлю себе на воображаемую"
 , "руку крестик, а потом вам напомню (но эта функция лишь в "
 , "разработке, пока лишь я  могу записывать все ваши дела и "
 , "выводить их)"
 , ""
 , "Принять у Родиона этот проект (Ведь анекдоты это круто!)"
 , ""
 , "Посоветовать фильм (обожаю линейку фильмов про терминатора"
 , "особенно тот момент, когда скайнет победил)"
 , ""
 , "Сыграть с вами в города (я только учусь, поэтому поддайся"
 , "пожалуйста)"
 ]


startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/anec (анекдот)" ]
      , [ "/list какое-то действие" ]
      , [ "/show (покажет список действий)"]
      , [ "/remove действие, раннее уже записанное"]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

anecMessage :: Text
anecMessage = Text.unlines
 [ "Пожалуйста выберите категорию анекдотов из предложенного." ]

-- | A start keyboard with some helpful todo suggestions.
anecMessageKeyboard :: Telegram.ReplyKeyboardMarkup
anecMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/Armyane и нарды", "/Shlyapa, которая как раз", "/Poruchic Ржевский" ]
      , [ "/Medved сел в машину и сгорел", "/Zashli в бар...", "/Raznoe" ]
      , [ "/Randomniy анекдот" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model "" [""]
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
   $ Anec       <$  command "anec"
 <|> Randomniy  <$  command "Randomniy"
 <|> Raznoe     <$  command "Raznoe"
 <|> Zashli     <$  command "Zashli"
 <|> Medved     <$  command "Medved"
 <|> Poruchic   <$  command "Poruchic"
 <|> Shlyapa    <$  command "Shlyapa"
 <|> Armyane    <$  command "Armyane"
 <|> ShowItems  <$  command "show"
 <|> RemoveItem <$>  command "remove"
 <|> GetTime    <$  command "time"
 <|> Start      <$  command "start"
 <|> AddItem    <$>  command "list"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    DoNothing -> pure model
    AddItem title  -> addItem title model <# do
      replyText "Добавил"
      pure DoNothing
      
    ShowItems -> model <# do
      replyText (showItems (todoItems model))
      pure DoNothing

    GetTime -> model <# do
      now <- liftIO getCurrentTime
      replyText (Text.pack (show now))
      pure DoNothing

    RemoveItem item ->
      case removeItem item model of
        Left err -> model <# do
          replyText err
          pure DoNothing
        Right newModel -> newModel <# do
          replyText "Убрал"
          pure ShowItems

    Start -> model <# do
      reply (toReplyMessage startMessage)
        { replyMessageReplyMarkup = Just
           (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        }
      pure DoNothing

    Anec -> model <# do
      reply (toReplyMessage anecMessage)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup anecMessageKeyboard)
        }
      pure DoNothing

    Armyane -> model <# do
      rand <- liftIO (randomRIO (1::Integer, 22))
      armyan <- liftIO (readFile (show rand ++ ".txt"))
      let mes = Text.pack armyan
      let aue = Text.unlines [ mes ]
      reply (toReplyMessage aue)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Shlyapa -> model <# do
      rand <- liftIO (randomRIO (69::Integer, 78))
      shlyapa <- liftIO (readFile (show rand ++ ".txt"))
      let mes1 = Text.pack shlyapa
      let aue1 = Text.unlines [ mes1 ]
      reply (toReplyMessage aue1)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Poruchic -> model <# do
      rand <- liftIO (randomRIO (45::Integer, 56))
      poruchic <- liftIO (readFile (show rand ++ ".txt"))
      let mes2 = Text.pack poruchic
      let aue2 = Text.unlines [ mes2 ]
      reply (toReplyMessage aue2)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Medved -> model <# do
      rand <- liftIO (randomRIO (23::Integer, 44))
      medved <- liftIO (readFile (show rand ++ ".txt"))
      let mes3 = Text.pack medved
      let aue3 = Text.unlines [ mes3 ]
      reply (toReplyMessage aue3)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Zashli -> model <# do
      rand <- liftIO (randomRIO (79::Integer, 90))
      zashli <- liftIO (readFile (show rand ++ ".txt"))
      let mes4 = Text.pack zashli
      let aue4 = Text.unlines [ mes4 ]
      reply (toReplyMessage aue4)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Raznoe -> model <# do
      rand <- liftIO (randomRIO (57::Integer, 68))
      raznoe <- liftIO (readFile (show rand ++ ".txt"))
      let mes5 = Text.pack raznoe
      let aue5 = Text.unlines [ mes5 ]
      reply (toReplyMessage aue5)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

    Randomniy -> model <# do
      rand <- liftIO (randomRIO (1::Integer, 90))
      randomniy <- liftIO (readFile (show rand ++ ".txt"))
      let mes6 = Text.pack randomniy
      let aue6 = Text.unlines [ mes6 ]
      reply (toReplyMessage aue6)
        { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
        } 
      pure DoNothing

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault (conversationBot Telegram.updateChatId (bot))) env
 
-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
{-# LANGUAGE OverloadedStrings #-}
main = run "1779431157:AAFbPHcUbXLhpFbY4H4TcByNBaf5NnrX9yI"

