# lottery for matching five numbers from 90 (Hungarian traditional lottery)
# a simple simulation

# price of one lottery in HUF 
lottery_price <- 250 

# main function
playgame <- function(extrashock=FALSE,fastplay=FALSE) {
    week <- 0
    # initial budget of lottery company
    company_budget <- 0
    # your initial budget
    your_budget <- 250
    # draw numbers week to week new numbers
    draw_numbers <- c(0,0,0,0,0)
    ## Your winner numbers :) I do not chage them
    secret_numbers <- sort(sample(seq(1,90),5))

    while (your_budget>-1000000000) {
        week  <- week + 1
        company_budget <- company_budget + lottery_price

        # shocking message
        if (!fastplay) { print(paste(week,'week')) }
        draw_numbers <- sort(sample(seq(1,90),5))
        if (all(secret_numbers[1:2]==draw_numbers[1:2])) { 
            print('You have 2 matches (money for ~six lotteries)')
            your_budget <- your_budget+lottery_price*6
            if (!question(your_budget)) {
                print(paste("Your budget is",your_budget))
                break
            }
        }
        if (all(secret_numbers[1:3]==draw_numbers[1:3])) { 
        #else if (all(secret_numbers[1:3]==my_numbers[1:3])) { 
            print('You have 3 matches (money for ~100 lotteries)') 
            your_budget <- your_budget+lottery_price*100
            if (!question(your_budget)) {
                print(paste("Your budget is",your_budget))
                print(paste("Company's budget is",company_budget))
                break
            }
        }
        else if (all(secret_numbers[1:4]==draw_numbers[1:4])) { 
            print('You have 4 matches (money for ~6000 lotteries)') 
            your_budget <- your_budget+lottery_price*6000
            if (!question(your_budget)) {
                print(paste("Your budget is",your_budget))
                print(paste("Company's budget is",company_budget))
                break
            }
        }
        else if (all(secret_numbers==draw_numbers)) { 
            print ("YOU WON!!!" )
            print(paste("Your budget is",your_budget))
            print(paste("Company's budget is",company_budget))
            break
        }
        else {
            your_budget <- your_budget - lottery_price
            # very shocking messages, suppress them if don't like the Truth
            if (extrashock) {
                print(paste("        nothing, you spent",your_budget,"for lotteries."))
            }
        }
    }
}

# question for exit from the game if you won something
question <- function(your_budget) {
    x <- ''
    while (x=='') {
        print(paste("Your budget is",your_budget,"Do you want to continue? (yes|no)"))
        con <- file("stdin")
        x <- readLines(con,1)
        if (x=='yes') {
            close(con)
            return(TRUE)
        }
        if (x=='no') {
            close(con)
            return(FALSE)
        }
        close(con)
        x <- ''
    }
}

# supress the very shocking messages 
tryCatch(stop(playgame(extrashock <- FALSE)),error=function(e) {print('Ok. Finished')}, finally = print("Can't you beleive it? Hah! Go and by lotteries :)"))
