#Parent Roll Selection Integer Program# -- Before y constraint removal

#define number of potential parent rolls and products
#number of parents, 1...R
numR <- length(rWidth) 
#number of products, 1...P
numP <- nrow(pWidth) 

#create Objective function coefficient matrix
#x_1,1....x_1,P x_2,1...x_2_P.....x_R,1...x_R,P z1...zR y1...yR
#x_r,p --> if 1 then product p is assigned to parent r, 0 otherwise 
#zr --> if 1 then roll r is a parent roll 
#yr --> integer count of number of products assigned to parent roll r 
obj <- rep_len(c(0),numR*numP+2*numR)
for (r in 1:numR){
  #index for row to update
  rstartrow <- (r-1)*numP
  for (p in 1:numP) { 
    #check if product p can be made by parent r 
    if (pWidth$width[p]>rWidth[r]) {next}
    #f_rp = number of tons of roll r required to make one ton of p 
    #use this f_rp when you can double cut from parent roll 
    f_rp <- 1 + ((rWidth[r]%%pWidth$width[p])/pWidth$width[p])
    #use this f_rp when you cannot double cut from parent roll 
    #f_rp <- 1 + ((rWidth[r]-pWidth$width[p])/pWidth$width[p])
    #trim loss associated with production of p with r 
    c_rp <- (f_rp-1)
    #total trim making p with r 
    trimcost <- c_rp*pWidth$dmd[p]
    #update obj function coefficient matrix
    obj[rstartrow+p] <- trimcost
  }
}

#create coefficient matrix for constraints
#columns are x_1,1..x_1,P x_2,1...x_2,P....x_R,1...x_R,P z1...zR y1...yR
mat <- matrix(data=0, nrow= numR+numP+numR+1, ncol = numR*numP+2*numR)
#constraints 1...R
#constraint that counts number of products made by each parent roll
#sum(-x_r,p) over p in P + yr = 0 
for (r in 1:numR){
  for (p in 1:numP) {
    #check if parent r can make p 
    if(pWidth$width[p]>rWidth[r]) {next}
    #index for what column to update
    pCol <- (r-1)*numP+p
    #update constraint matrix 
    #set coefficient for x_r,p to -1 
    mat[r,pCol] <- -1
    #set coefficient of yr to 1 
    mat[r,numR*numP+numR+r] <- 1 
  }
}
#constraints R+1...R+P
#constraint that there is only one parent roll per product
#sum(x_rp) over r in Rp = 1 
for (p in 1:numP){
  for (r in 1:numR){
    #check if product p can be made from parent r 
    if (rWidth[r]<pWidth$width[p]) {next}
    #index for what column to update
    rCol <- p+numR*(r-1)
    #update constraint matrix
    #set coefficient for x_r,p to 1 
    mat[(numR+p),rCol] <- 1 
  }
}
#constraints R+P+1...R+P+R
#constraint that determines if roll r was used
#sum(-x_r,p)+bigM*zr >= 0 
for (r in 1:numR){
  for(p in 1:numP){
    #check if product p can be made from r 
    if(pWidth$width[p]>rWidth[r]) {next}
    #index for what column to update
    pCol <- (r-1)*numP+p 
    #udpate constraint matrix 
    #set coefficient for zr to bigM
    #bigM defined in User Inputs.R
    mat[(numR+numP+r),(numP*numR+r)] <- bigM
    #set coefficient for x_r,p to -1
    mat[(numR+numP+r),pCol] <- -1 
  }
}
#constraint R+P+R+1
#constraint limiting selected number of parent rolls
#sum(zr) over r in R = number of parent rolls 
for (r in 1:numR) { 
  #set coefficient of zr to 1 
  mat[(numR+numP+numR+1),numR*numP+r]<- 1
}
#constraints to limit trim size 
#will force x_r,p to 0 if p is smaller than r by more than trimLimit

#add a new row for a constraint
mat <- rbind(mat,c(rep_len(0,numR*numP+2*numR)))
#index for what row to update
rowNum <- nrow(mat)
for (r in 1:numR){
  for (p in 1:numP) {
    #check if product p can be made from r
    if (pWidth$width[p]>rWidth[r]) {next}
    #check if trim exceeds trim limit
    #trimLimit defined in User Inputs.R
    if (rWidth[r]%%pWidth$width[p]<trimLimit) {next}
    #update constraint matrix
    #set coefficient of x_r,p to 1 
    mat[rowNum,numP*(r-1)+p] <- 1 
  }
}


#constraint to adhere to cutter sizes in converting plants
#will force x_r,p to 0 if p cannot be cut from r because of a cutter size
#will add row in matrix if constraint is necessary 
#cutters list defined in User Inputs.R, list of sizes in inches
#add new row in constraint matrix 
mat <- rbind(mat,c(rep_len(0,numR*numP+2*numR)))
#index for what row to update
rowNum <- nrow(mat)
for (cutter in cutters) { 
  for (r in 1:numR){
    #check that parent is above cutter size 
    if (rWidth[r]<=cutter) {next}
    #check that parent is within trimLimit of cutter size
    #trimLimit defined in User Inputs.R, same as constraint above
    if (rWidth[r]>cutter+trimLimit) {next}
    for(p in 1:numP){
      #check if product p can be made from parent r 
      if(pWidth$width[p]>rWidth[r]) {next}
      #check if product p is within trimLimit of cutter size
      #same trimLimit as above, defined in User Inputs.R
      if(pWidth$width[p]<=cutter-trimLimit) {next}
      #check that product p is smaller than cutter
      if(pWidth$width[p]>cutter) {next}
      
      #set coefficient of x_r,p to 1 
      mat[rowNum,numP*(r-1)+p]<-1
    }
  }  
}
#constraint to adhere to slitter capabilities in converting plants
#slitter_limits_list created in Data Read In.R 
#will force x_r,p to 0 if p cannot be slitted from r 
#will add new row to constraint matrix if constraint is necessary
#if so, add new row to constraint matrix 
mat <- rbind(mat,c(rep_len(0,numR*numP+2*numR)))
#index for what row to update
rowNum <- nrow(mat)
for (r in 1:numR){
  for (p in 1:numP){
    #check that product p can be made from parent r 
    if(pWidth$width[p]>rWidth[r]) {next}
    #check that product p can be double cut from parent r 
    if(rWidth[r]/pWidth$width[p]<2) {next}
    #check if product is part of list that doesn't allow slitting
    if(pWidth$width[p] %in% slitter_limits_list == FALSE) {next}
    #set coefficient of x_r,p to 1 
    mat[rowNum,numP*(r-1)+p] <- 1
  }
}

#counts the number of trimLimit, cutter, and slitter constraints
numLastConstraint <- nrow(mat) - (numR+numP+numR+1)

#define constraint operators 
dir <- c(rep_len("==",numR),rep_len("==",numP),rep_len(">=",numR),"==",
         rep_len("==",numLastConstraint))
#define variable types 
#x_1,1...x_R,P are binary, z1..zR are binary, y1...yR are integer
types<-c(rep_len("B",numR*numP),rep_len("B",numR),rep_len("I",numR))
#define objective type (TRUE=maximum, FALSE=minimum)
max <- FALSE

#create empty data frame for roll selection options  
RollSelectionOptions <- data.frame("Width" = c(0), "Parent" = c(0),
                                   "# Parents" = c(0),"Trim Tons" = c(0),
                                   "Marginal Benefit"=c(0))

#create counter for possible scenarios
scenario <- 0 
#run selection optimization problem for 1 roll through all rolls
for (r in 1:numR){   #1:numR){
  #define number of parents 
  numParents <- r
  #define right hand side of constraint equations 
  #constraints 1..R = 0, R+1..R+P = 1, R+P+1...R+P+R >= 0,  
  #R+P+R+1 = 0, remaining = 0 
  rhs <- c(rep_len(0,numR),rep_len(1,numP),rep_len(0,numR),numParents, 
           rep_len(0,numLastConstraint))
  #solve optimization problem 
  sol <- Rsymphony_solve_LP(obj, mat, dir, rhs, types= types, max=max)
  print(c(r,"Solved"))
  
  #define assignment matrix 
  assignment <- matrix (sol$solution, nrow=numP)
  #if solution is impossible the sume of assignment == 0
  if(sum(assignment)==0) {next}
  #if solution is possible, add to scenario count
  scenario <- scenario + 1 
  #add solutions to list of roll selection options
  #for each product
  for(p in 1:numP){
    #add row to dataframe
    RollSelectionOptions <- rbind(RollSelectionOptions,c(0,0,0,0,0))
    #index for which row to update
    numRow <- nrow(RollSelectionOptions)
    #set width to product width
    RollSelectionOptions$Width[numRow] <- pWidth$width[p]
    #set number of parents 
    RollSelectionOptions$X..Parents[numRow] <- numParents
    #set trim tons to objective value 
    RollSelectionOptions$Trim.Tons[numRow] <- sol$objval
    #calculate marginal benefit (number of trim tons better than 
    # trim tons from number of parents - 1) 
    RollSelectionOptions$Marginal.Benefit[numRow] <- if (scenario>1) 
      (RollSelectionOptions$Trim.Tons[numRow-numP]-
         RollSelectionOptions$Trim.Tons[numRow]) else 
           RollSelectionOptions$Trim.Tons[numRow]
    #find parent product p is assigned to 
    #create empty parent list 
    parent_list <- c(rep_len(0,numP))
    #for each element in parent list 
    for (x in 1:numP){ 
      #if assignment (x_r,p) is 1 then save parent index number
      parent_list[x] <- if (assignment[p,x]>0) x else 0
    }
    #set parent of product
    RollSelectionOptions$Parent[numRow] <- rWidth[sum(parent_list)]
  }
}

#add information about grade and caliper 
RollSelectionOptions <- cbind("Grade"= one_parent_grade, 
                              "Caliper"= one_parent_caliper, RollSelectionOptions)
#formatting to get rid of first row of zeros 
RollSelectionOptions <- RollSelectionOptions[-1,]