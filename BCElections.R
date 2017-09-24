library(readxl)
library(data.table)
library(ggplot2)
library(ggtern)

BC2013 <- data.table(read_excel("2013_GE_Final_VA_Results_data.xlsx", sheet = "DATA"))
BC2013 <- BC2013[, .(count=sum(count)), by=c("ED_code","ED_name","ballot_name","party_affiliation")]
BC2013 <- BC2013[, .(Riding=ED_name, Candidate=ballot_name, Party=party_affiliation, Votes=as.integer(count))][Candidate != "REJECTED BALLOTS"]

BC2017 <- data.table(read_excel("2017_GE_Interim_VA_Results_summary.xlsx", sheet = "Summary_by_candidate"))
BC2017 <- BC2017[1L:(nrow(BC2017)-2)]
BC2017[, `Electoral district` := Reduce(function(a, b) if (is.na(b)) a else b, BC2017$`Electoral district`, NA, accumulate=TRUE)[2L:(.N+1L)]]
BC2017 <- BC2017[, .(Riding=`Electoral district`, Candidate=`Candidate ballot name`, Party=`Party affiliation`, Votes=as.integer(`Valid votes`))]

RidingInfo2017 <- fread("RidingRegions2017.csv")[fread("RidingCodes2017.csv"), on="Code"]
setkey(RidingInfo2017, Riding)
setorder(RidingInfo2017, Riding)

BCElections <- rbind(BC2013[, c(.(Year="2013"), .SD)], BC2017[, c(.(Year="2017"), .SD)])
BCElections[, Party := ifelse(is.na(Party), "Independent", Party)]
BCElections <- BCElections[, .SD[order(-Votes)][, c(.SD, .(Position=1L:.N))], by=c("Year", "Riding")]
BCSummaries <- BCElections[, {
  Summary <- .SD[, .(PopularVote=sum(Votes)), by="Party"]
  Candidates <- .SD[, .(Candidates=.N), by="Party"]
  TotalVotes <- sum(Summary$PopularVote)
  Seats <- .SD[, .(Seats=.SD[Position==1L, .N]), by=c("Party")]
  Summary[, PopularVote_Percent := 100 * PopularVote / TotalVotes][Candidates, on="Party"][Seats, on="Party"]
}, by="Year"]

setorder(BCSummaries, Year, -PopularVote)

data <- BCElections[
  , .(LIB=.SD[Party=="BC Liberal Party", Votes],
      NDP=.SD[Party == "BC NDP", Votes],
      GRN=.SD[Party == "BC Green Party", Votes])
  , by=c("Year","Riding")]

f <- function(x) ifelse(is.na(x), NA, x)
data[, `:=`(LIB=f(LIB), NDP=f(NDP), GRN=f(GRN))]
data[, Winner := c("LIB","NDP","GRN")[which.max(c(LIB, NDP, GRN))] , by=c("Year", "Riding")]
data[, Winner.2017 := .SD[Year=="2017", Winner], by=Riding]
#data[Year=="2013", Riding := { R <- Riding; Predecessors[R, ]$Riding }, by="Riding"]
data[, Flipped := length(unique(Winner)) > 1, by=c("Riding")]
data <- RidingInfo2017[data, on="Riding"]
ranks <- data[Year=="2017", .(Riding, GRN=GRN/(LIB+GRN+NDP))][, .(Riding, rank = as.integer(rank(GRN)))]
setkey(ranks, Riding)
data <- merge(data, ranks, by="Riding")
data[Riding=="Cariboo-Chilcotin", `:=`(Region="North BC Interior", Code="CBC")]

data[, Kind := ifelse(Flipped, "Gain", "Hold")]

regional_swings <- data[
  , .(Riding=Region, Code="RS", Winner="RS", Winner.2017="RS", Flipped=TRUE, Kind="Regional Swing", rank=0,
      LIB=sum(LIB, na.rm=TRUE), NDP=sum(NDP, na.rm=TRUE), GRN=sum(GRN, na.rm=TRUE)),
  by=c("Year", "Region")][order(Year)]

ggtern(aes(x=NDP, y=GRN, z=LIB, group=Riding, col=Winner), data=data) +
  geom_path(aes(alpha=Kind, col=Winner.2017, size=Kind), arrow=arrow(length=unit(0.25, "cm"))) +
  scale_alpha_manual(values=c("Hold"=0.5, "Gain"=1, "Regional Swing"=1)) +
  scale_size_manual(values=c("Hold"=0.3, "Gain"=0.6, "Regional Swing"=1.2)) +
  geom_point(aes(shape=Year), size=1.5) +
  scale_shape_manual(values=c("2013"=22, "2017"=19)) +
  geom_text(aes(label=Code), size=1.5, hjust="inward", vjust="inward", col="black") +
  scale_color_manual(values=c(LIB="#FF0000", NDP="#FF8800", GRN="#00AA00", RS="#000000", "Gain"="#444444", "Regional Swing"="#000000")) +
  facet_wrap(~Region)

ggtern(aes(x=LIB, y=GRN, z=NDP, shape=Year, group=Region), data=regional_swings) + geom_point() + geom_path() + facet_wrap(~Region)