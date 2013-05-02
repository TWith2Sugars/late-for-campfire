module Twitter

open System
open System.IO
open TweetSharp

type Tweet = { Text:string; User:string }

let private path = "last-tweet-id.txt"

let private fetchLastTweetId = 
    if not (File.Exists path) then 
        None
    else
        let text = File.ReadAllText path
        let success, id = Int64.TryParse text
        if success then Some(id)
        else None

let private setLastTweetId (tweet:TwitterStatus) =
    File.WriteAllText(path, tweet.Id.ToString())

let private getMentions (twitterService : TwitterService) = 
    let options = ListTweetsMentioningMeOptions()
    match fetchLastTweetId with
    | Some id -> options.SinceId <- Nullable<int64> id
    | None -> ()    

    let mentions = twitterService.ListTweetsMentioningMe options
    if not (Seq.isEmpty mentions) then
        mentions 
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

let private matchTweet (tweet:TwitterStatus) (list:TwitterCursorList<TwitterUser>) =
    list |> Seq.exists (fun u -> u.Id = tweet.User.Id)

let getTweets consumerKey consumerSecret accessToken accessSecret user listName = 
    let twitterService = TwitterService(consumerKey, consumerSecret, accessToken, accessSecret)
    let tweets = getMentions twitterService
    let userList = getRestrictedUsers twitterService user listName
    tweets 
    |> Seq.filter (fun x -> matchTweet x userList)
    |> Seq.map (fun x -> {Text=x.Text; User=x.User.Name})