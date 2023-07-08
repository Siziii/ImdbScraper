{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import System.IO
import Data.Char (isAscii)

main :: IO ()
main = do
  putStrLn "------------------------------------"
  putStrLn "  Welcome to the IMDb Top Lists!"
  putStrLn "------------------------------------"
  loop

loop :: IO ()
loop = do
  putStrLn "------------------------------------"
  putStrLn "  Choose an option:"
  putStrLn "------------------------------------"
  putStrLn "  1. Show top 100 Movies"
  putStrLn "  2. Show top 100 TV Shows"
  putStrLn "  3. Show top 100 Lowest Rated Movies"
  putStrLn "  e. Exit"
  choice <- getLine
  case choice of
    "1" -> do
      scrapeAndShowTopMovies
      loop
    "2" -> do
      scrapeAndShowTopTVShows
      loop
    "3" -> do
      scrapeAndShowLowestRatedMovies
      loop
    "e" -> putStrLn "Exiting..."
    _   -> do
      putStrLn "Invalid choice. Please try again."
      loop

scrapeAndShowTopMovies :: IO ()
scrapeAndShowTopMovies = do
  let url = "https://www.imdb.com/chart/top"
  movies <- scrapeURL url movieScraper
  case movies of
    Just results -> do
      putStrLn "Top 100 Rated Movies:"
      let formattedResults = formatResults (take 100 (addNumbering results))
      writeFile "top_movies.txt" formattedResults
      putStrLn formattedResults
    Nothing -> putStrLn "Failed to scrape IMDb top movies."

scrapeAndShowTopTVShows :: IO ()
scrapeAndShowTopTVShows = do
  let url = "https://www.imdb.com/chart/toptv"
  tvShows <- scrapeURL url tvShowScraper
  case tvShows of
    Just results -> do
      putStrLn "Top 100 Rated TV Shows:"
      let formattedResults = formatResults (take 100 (addNumbering results))
      writeFile "top_tv_shows.txt" formattedResults
      putStrLn formattedResults
    Nothing -> putStrLn "Failed to scrape IMDb top TV shows."

scrapeAndShowLowestRatedMovies :: IO ()
scrapeAndShowLowestRatedMovies = do
  let url = "https://www.imdb.com/chart/bottom"
  movies <- scrapeURL url movieScraper
  case movies of
    Just results -> do
      putStrLn "Top 100 Lowest Rated Movies:"
      let formattedResults = formatResults (take 100 (addNumbering results))
      writeFile "lowest_rated_movies.txt" formattedResults
      putStrLn formattedResults
    Nothing -> putStrLn "Failed to scrape IMDb lowest rated movies."

-- Define the scraper to extract movie titles and ratings
movieScraper :: Scraper String [(String, Double)]
movieScraper = chroots ("tbody" // "tr") $ do
  title <- text $ "td" @: [hasClass "titleColumn"] // "a"
  rating <- text $ "td" @: [hasClass "ratingColumn", hasClass "imdbRating"] // "strong"
  return (title, read rating)

-- Define the scraper to extract TV show titles and ratings
tvShowScraper :: Scraper String [(String, Double)]
tvShowScraper = chroots ("tbody" // "tr") $ do
  title <- text $ "td" @: [hasClass "titleColumn"] // "a"
  rating <- text $ "td" @: [hasClass "imdbRating"] // "strong"
  return (title, read rating)

-- Format the results as a string with numbering
formatResults :: [(String, Double)] -> String
formatResults results = unlines (map formatMovie results)

-- Format a single movie or TV show as a string
formatMovie :: (String, Double) -> String
formatMovie (title, rating) = filter isAscii title ++ " - " ++ show rating

-- Add numbering to the list of results
addNumbering :: [(String, Double)] -> [(String, Double)]
addNumbering = zipWith addNumber [1..]
  where
    addNumber position (title, rating) = (show position ++ ". " ++ title, rating)
