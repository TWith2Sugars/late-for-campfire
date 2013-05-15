module Twitter

open System
open System.IO
open TweetSharp

type Tweet = { Text:string; User:string; Id:int64; UserId:int64 }

let private path = "last-tweet-id.txt"

let private fetchLastTweetId = 
    if not (File.Exists path) then 
        None
    else
        let text = File.ReadAllText path
        let success, id = Int64.TryParse text
        if success then Some(id)
        else None

let private setLastTweetId (tweet:Tweet) =
    File.WriteAllText(path, tweet.Id.ToString())

let private getMessages (twitterService : TwitterService) = 
    let mentionOptions = ListTweetsMentioningMeOptions()
    let directOptions = ListDirectMessagesReceivedOptions()
    match fetchLastTweetId with
    | Some id -> mentionOptions.SinceId <-  Nullable<int64> id; directOptions.SinceId <-  Nullable<int64> id
    | None -> ()    

    let mentions = 
        twitterService.ListTweetsMentioningMe mentionOptions
        |>  Seq.map (fun x -> {Text=x.Text; User=x.User.Name; Id=x.Id; UserId=x.User.Id}) 

    let direct = 
        twitterService.ListDirectMessagesReceived directOptions
        |>  Seq.map (fun x -> {Text=x.Text; User=x.Sender.Name; Id=x.Id; UserId=x.Sender.Id}) 
    
    let messages = [ mentions; direct] |> Seq.concat
    if not (Seq.isEmpty messages) then
        messages
        |> Seq.maxBy (fun x -> x.Id) 
        |> setLastTweetId

    mentions

let private getRestrictedUsers (twitterService : TwitterService) user listName =
    let options = GetListOptions()
    options.OwnerScreenName <- user
    options.Slug <- listName
    let restrictedList = twitterService.GetList options
    let membershipOptions = ListListMembersOptions()
    membershipOptions.ListId <- System.Nullable<int64> restrictedList.Id
    twitterService.ListListMembers membershipOptions

let private matchTweet (tweet:Tweet) (list:TwitterCursorList<TwitterUser>) =
    list |> Seq.exists (fun u -> u.Id = tweet.UserId)

let getTweets consumerKey consumerSecret accessToken accessSecret user listName = 
    let twitterService = TwitterService(consumerKey, consumerSecret, accessToken, accessSecret)
    let tweets = getMessages twitterService
    let userList = getRestrictedUsers twitterService user listName
    tweets 
    |> Seq.filter (fun x -> matchTweet x userList)