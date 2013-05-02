module Arguments

// set up a type to represent the options
type CommandLineOptions = {
    campfireApi: string; 
    campfireRoom: string; 
    twitterConsumerKey: string; 
    twitterConsumerSecret: string; 
    twitterAccessToken: string; 
    twitterAccessSecret: string; 
    twitterListUser: string; 
    twitterListName: string; 
    }

// create the "helper" recursive function
let rec private parseCommandLineRec args optionsSoFar = 
    match args with 
    // empty list means we're done.
    | [] -> 
        optionsSoFar  
            
    | "-campfireApi"::xs -> 
        let newOptionsSoFar = { optionsSoFar with campfireApi=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-campfireRoom"::xs -> 
        let newOptionsSoFar = { optionsSoFar with campfireRoom=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterConsumerKey"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterConsumerKey=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterConsumerSecret"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterConsumerSecret=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterAccessToken"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterAccessToken=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterAccessSecret"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterAccessSecret=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterListUser"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterListUser=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    | "-twitterListName"::xs -> 
        let newOptionsSoFar = { optionsSoFar with twitterListName=List.head xs}
        parseCommandLineRec (List.tail xs) newOptionsSoFar 

    // handle unrecognized option and keep looping
    | x::xs -> 
        eprintfn "Option '%s' is unrecognized" x
        parseCommandLineRec xs optionsSoFar 

// create the "public" parse function
let parseCommandLine args = 
    // create the defaults
    let defaultOptions = {
        campfireApi="";
        campfireRoom=""; 
        twitterConsumerKey=""; 
        twitterConsumerSecret=""; 
        twitterAccessToken=""; 
        twitterAccessSecret=""; 
        twitterListUser=""; 
        twitterListName=""; 
    }

    // call the recursive one with the initial options
    parseCommandLineRec args defaultOptions 
