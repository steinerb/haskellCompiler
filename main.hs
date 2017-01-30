--import lexer


main :: IO ()
main = do 
	message
	userInput <- getLine
	handle userInput

message :: IO ()
message = do putStrLn "Do you want to hear the message?:\n"

theMessage :: [Char]
theMessage = "It's like a jungle sometimes\nIt makes me wonder how I keep from goin' under\n\nBroken glass everywhere\nPeople pissin' on the stairs, you know they just don't care\nI can't take the smell, can't take the noise\nGot no money to move out, I guess I got no choice\nRats in the front room, roaches in the back\nJunkies in the alley with a baseball bat\nI tried to get away but I couldn't get far\nCause a man with a tow truck repossessed my car\n\nDon't push me cause I'm close to the edge\nI'm trying not to lose my head\nIt's like a jungle sometimes\nIt makes me wonder how I keep from goin' under\n\nStandin' on the front stoop hangin' out the window\nWatchin' all the cars go by, roarin' as the breezes blow\nCrazy lady, livin' in a bag\nEatin' outta garbage pails, used to be a fag hag\nSaid she'll dance the tango, skip the light fandango\nA Zircon princess see\nmed to lost her senses\nDown at the peep show\n watchin' all the creeps\nSo she can tell her stories to the girls back home\nShe went to the city and got so so seditty\nShe had to get a pimp, she couldn't make it on her own\n\nIt's like a jungle sometimes\nIt makes me wonder how I keep from goin' under\n\nMy brother's doin' bad, stole my mother's TV\nSays she watches too much, it's just not healthy\nAll My Children in the daytime, Dallas at night\nCan't even see the game or the Sugar Ray fight\nThe bill collectors, they ring my phone\nAnd scare my wife when I'm not home\nGot a bum education, double-digit inflation\nCan't take the train to the job, there's a strike at the station\nNeon King Kong standin' on my back\nCan't stop to turn around, broke my sacroiliac\nA mid-range migraine,\n cancered membrane\nSometimes I think I'm\n goin' insane\nI swear I might hijac\nk a plane\n\n\n\n\\nn\n!\n\nIt's like a jungle sometimes\nIt makes me wonder how I keep from goin' under\n\nA child is born with no state of mind\nBlind to the ways of mankind\nGod is smilin' on you but he's frownin' too\nBecause only God knows what you'll go through\nYou'll grow in the ghetto livin' second-rate\nAnd your eyes will sing a song called deep hate\nThe places you play and where you stay\nLooks like one great big alleyway\nYou'll admire all the number-book takers\nThugs, pimps and pushers and the big money-makers\nDrivin' big cars, spendin' twenties and tens\nAnd you'll wanna grow up to be just like them, huh\nSmugglers, scramblers, burglars, gamblers\nPickpocket peddlers, even panhandlers\nYou say I'm cool, huh, I'm no fool\nBut then you wind up droppin' outta high school\nNow you're unemployed, all non-void\nWalkin' round like you're Pretty Boy Floyd\nTurned stick-up kid, but look what you done did\nGot sent up for a eight-year bid\nNow your manhood is took and you're a Maytag\nSpend the next two years as a undercover fag\nBein' used and abused to serve like hell\nTil one day, you was found hung dead in the cell\nIt was plain to see that your life was lost\nYou was cold and your body swung back and forth\nBut now your eyes sing the sad, sad song\nOf how you lived so fast and died so young so\n\nIt's like a jungle sometimes\nIt makes me wonder how I keep from goin' under"

handle :: [Char] -> IO ()
handle "yes" = do putStrLn theMessage
handle "no" = print "FINE."
