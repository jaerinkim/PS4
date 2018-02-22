set.seed(1324)
wrongidentifier<-function(doorthing, doorthing2, x){
  browser()## Looks how 'doorthing' and 'doorthing2' look like.
  doorthing1<-doorthing2<-sample(1:3, 1)
  if (doorthing1==doorthing2){ x<-TRUE } else { x==FALSE }
  x
  browser() ## Looks how 'doorthing1' and 'doorthing2' look by the end of the function
  }

wrongidentifier(sample(1:3, 1), sample(1:3, 1))

## It turns out that 'doorthing' is assigned 1, 'doorthing2' is assigned 2
## at the beginning of the function. Yet, the next line (line 4) overrides 'doorthing2'
## such that 'doorthing1' and 'doorthing2' always equal (with this seed, they are assigned 3).
## Moreover, 'doorthing' is not even used at all.
## Then this function always returns 'TRUE', which is not our original intention.

## New identifier

identifier<-function(doorchosen,seedno){
    set.seed(seedno)  ## Sets seed number.
    return(doorchosen==sample(1:3,1)) ##Returns TRUE if the door chosen equals the sampled number.
}

identifier(1,3255)
identifier(2,3255)
identifier(3,3255)
## One of them should return TRUE, two of them FALSE, as they do.


## Generating the door class.
door<-setClass(Class="door",
         ## Four slots require two of them to be Boolean and the other to be integers.
               slots=c(carDoor="numeric",
                 switch="logical",
                 chosenDoor="numeric",
                 winner="logical"),
         prototype=list(switch=FALSE, winner=FALSE),
         ## Only 'switch' and 'winner' are assigned the default value. The rest should be
         ## set in other instances.
         
         ## Checks the validity of the function.
         validity=function(object){
          validdoor=c(1,2,3) ## Valid door is a set (vector) of three numbers, 1, 2, and 3.
           if(sum(object@carDoor==validdoor)!=1){       
             ## If 'carDoor' does not contain only one of the elements of the validdoor,
             return("Wrong number assigned to carDoor")
           }
          if(sum(object@chosenDoor==validdoor)!=1){
            ## If 'chosenDoor' does not contain only one of the elements of the validdoor,
            return("Wrong number assigned to chosenDoor")}
         })
## Does the validity check work as it should?
testdoor<-door(carDoor=4,chosenDoor=-1.1)
## Maybe something's wrong with carDoor
testdoor<-door(carDoor=2,chosenDoor=-1.1)
## Maybe something's wrong with chosenDoor
testdoor<-door(carDoor=2,chosenDoor=3)
## Now it works.


## Generating a generic function, 'doorgen', which has 'newswitch' as an input,
## and has randomized 'carDoor' and 'chosenDoor', and a Boolean 'switch' as outputs
setGeneric("doorgen",
           function(newswitch){
             newdoor<-new("door",carDoor=sample(c(1:3),1), 
                          chosenDoor=sample(c(1:3),1),
                          switch=newswitch #The only parameter of the function,
                          ## choosing the value of 'switch'
                          )
             return(newdoor)
           })
set.seed(1512)
testdoor2<-doorgen(TRUE)
testdoor2
## An object, 'testdoor2', with class of "door" was created, with switch on.


## Generating the function, 'PlayGame'
## This generic function is empty, since we have no use for it.
setGeneric("PlayGame",
           function(object){print("This game is not for you.")})
PlayGame(0)
class(0)
## No, this game is not for a numeric.

## We are more interested in 'door' method of the PlayGame function.

setMethod("PlayGame", "door",

            function(object){
            object@winner<-FALSE
            object@carDoor<-sample(c(1:3),1) ## Overrides previously assigned 'carDoor'
            if(object@switch==TRUE){## If 'switch' is TRUE,
              ## see if carDoor==chosenDoor
              if(object@carDoor==object@chosenDoor){
                ## If so, sample among where carDoor==chosenDoor is, and another door.
                object@chosenDoor<-sample(c(object@carDoor,sample(c(1:3)[object@carDoor!=c(1:3)],1)),1)
              }
              else{
                ## If not, sample among carDoor and chosenDoor (the other door will be removed anyway.)
                object@chosenDoor<-sample(c(object@carDoor,object@chosenDoor),1)
              }
              ## In fact, in both cases, what we are doing effectively is choosing between the carDoor
              ## and a non-carDoor. Naturally, we can expect that the probability that carDoor=chosenDoor
              ## should be .5
            }
            ## If switch==FALSE, chosenDoor remains the same throughout the game.
            else{object@chosenDoor<-sample(c(1:3),1)}
            
            ## Now we determine if the player won.
            if(object@carDoor==object@chosenDoor){
              object@winner<-TRUE
              ## print("You won! Statistics is as follows.")
              ## omitted for the next task
            }
            
              return(object)

          })
testdoor<-doorgen(FALSE)
PlayGame(testdoor)
PlayGame(testdoor2)

## Interestingly, the player who chose to switch lost (testdoor2)
## and the one who chose not to won (testdoor1).
## What if we ran the test 1000 times?

cdoor.result<-mean(sapply(1:1000,function(x)PlayGame(doorgen(TRUE))@winner))
ncdoor.result<-mean(sapply(1:1000,function(x)PlayGame(doorgen(FALSE))@winner))
cdoor.result
ncdoor.result
cdoor.result-ncdoor.result

## As expected, changing door has frequency close to the theoretical expectation, .5.
## Not changing door has frequency close to the theoretical expectation, 1/3.
## Then the former is the better strategy than the latter.
## Yet, the best strategy will be to uncoditionally switch, rather than to switch half of the time.
## 1/3 of the time, the car will be behind the door the player has chosen,
## where changing the door always loses the game.
## For 2/3 of the time, however, the car will always be on the remaining door,
## other than the open one and the one the player is standing by.
## Then, unconditionally switching will grant the player 2/3 chance to win,
## and will be the best strategy.