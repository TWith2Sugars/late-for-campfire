module Twitter

open System
open System.IO
open TweetSharp

type Tweet = { Text:string; User:string; Id:int64; UserId:int64 }

let private lastMention = "last-tweet-id.txt"
let private lastDirectMessage = "last-direct-tweet-id.txt"

let private fetchLastTweetId path = 
    if not (File.Exists path) then 
        Nullable<int64>()
    else
        let text = File.ReadAllText path
        let success, id = Int64.TryParse text
        if success then Nullable<int64>(id)
        else Nullable<int64>()

let private setLastTweetId (path, tweetId:int64) =
    File.WriteAllText(path, tweetId.ToString())

let private setLastDirectMessageId (tweet:TwitterDirectMessage) = 
    setLastTweetId(lastDirectMessage, tweet.Id)

let private setLastMentionMessageId (tweet:TwitterStatus) = 
    setLastTweetId(lastMention, tweet.Id)

let private getMessages (twitterService : TwitterService) = 
    let mentionOptions = ListTweetsMentioningMeOptions()
    mentionOptions.SinceId <- fetchLastTweetId lastMention

    let directOptions = ListDirectMessagesReceivedOptions()
    directOptions.SinceId <- fetchLastTweetId lastDirectMessage
       
    let rawMentions = 
        match twitterService.ListTweetsMentioningMe mentionOptions with
        | null -> Seq.empty<TwitterStatus>
        | x -> x

    let mentions = 
        if Seq.isEmpty rawMentions then
            Seq.empty<Tweet>
        else
            rawMentions
            |> Seq.maxBy (fun x -> x.Id) 
            |> setLastMentionMessageId
            |> ignore

            rawMentions 
            |> Seq.map (fun x -> {Text=x.Text; User=x.User.Name; Id=x.Id; UserId=x.User.Id})

    let rawDirect = 
        match  twitterService.ListDirectMessagesReceived directOptions  with
        | null -> Seq.empty<TwitterDirectMessage>
        | x -> x

    let direct = 
        if Seq.isEmpty rawDirect then
            Seq.empty<Tweet>
        else
            rawDirect
            |> Seq.maxBy (fun x -> x.Id) 
            |> setLastDirectMessageId
            |> ignore

            rawDirect 
            |> Seq.map (fun x -> {Text=x.Text; User=x.Sender.Name; Id=x.Id; UserId=x.Sender.Id})
    
    [ mentions; direct] |> Seq.concat
   

let private getRestrictedUsers (twitterService : TwitterService) user listName =
    let options = GetListOptions()
    options.OwnerScreenName <- user
    options.Slug <- listName
    let restrictedListId = 
        match twitterService.GetList options with
        | null ->  System.Nullable<int64>()
        | x ->  System.Nullable<int64> x.Id

    let membershipOptions = ListListMembersOptions()
    membershipOptions.ListId <- restrictedListId
    twitterService.ListListMembers membershipOptions

let private matchTweet (tweet:Tweet) (list:TwitterCursorList<TwitterUser>) =
    list |> Seq.exists (fun u -> u.Id = tweet.UserId)

let getTweets consumerKey consumerSecret accessToken accessSecret user listName = 
    let twitterService = TwitterService(consumerKey, consumerSecret, accessToken, accessSecret)
    let tweets = getMessages twitterService
    let userList = getRestrictedUsers twitterService user listName
    tweets 
    |> Seq.filter (fun x -> matchTweet x userList)