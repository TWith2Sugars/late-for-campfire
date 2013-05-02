open Twitter
open Campfire
open Arguments

type NotHere = 
    | Late of Tweet
    | Ill of Tweet
    | Other of Tweet

let whyAreTheyNotHere (tweet:Tweet) = 
    match tweet.Text with
    | x when x.ToLowerInvariant().Contains("late") -> Late(tweet)
    | x when x.ToLowerInvariant().Contains("ill") -> Ill(tweet)
    | x -> Other(tweet)

let messageForCampfire notHere =
    match notHere with 
    | Late l -> l.User + " is running late"
    | Ill i -> "Poor " + i.User + " is ill and won't be in today :("
    | Other o -> o.User + " has messaged us; this is what they said: " + o.Text

[<EntryPoint>]
let main args =
    let arguments = Arguments.parseCommandLine (List.ofArray args)

    let campfire = Campfire.postMessage arguments.campfireRoom arguments.campfireApi
    Twitter.getTweets arguments.twitterConsumerKey arguments.twitterConsumerSecret arguments.twitterAccessToken arguments.twitterAccessSecret arguments.twitterListUser arguments.twitterListName
    |> Seq.map whyAreTheyNotHere
    |> Seq.map messageForCampfire
    |> campfire
    
    0 // return an integer exit code