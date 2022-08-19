###########
##Characterization of chicken farming system in Vietnam - script R
###########

require(tidyverse)
require(FactoMineR)
require(FactoInvestigate)
require(factoextra)
require(summarytools)
require(dplyr)
library(Factoshiny)
library(writexl)


#### 1) Realization of the typology of farming practices using MCA and HCA


Data = read.csv(
  "CleandataR.csv",
  sep = ";",
  h = T,
  stringsAsFactors = T
) %>% filter(ABgiven == "Yes") # select only the farms that use antibiotics


# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(Data, plain.ascii = F, trim.strings = T) %>%  view()

#Selection of the variables for recoding
TypoData = Data %>% select(
  Calculated_number,
  Numberflockbroiler,
  NumberflockPulletLHbreeding,
  NumberflockLHbreeding,
  NumberflockLHeggconsumption,
  NumberflockPulletLHeggconsumpt,
  Calculated_number,
  Numberflockbroiler,
  NumberflockPulletLHeggconsumpt,
  NumberflockLHeggconsumption,
  NumberflockPulletLHbreeding,
  NumberflockLHbreeding,
  Questionnairecode,
  ABgiven,
  Calculated_number,
  Estimatemortality,
  NewsystemStatus,
  Mainactivity,
  Otheranimal,
  Otheranimalspecie,
  TypeMainchicken,
  Otherchickenstype,
  Workingoccasionally,
  Otherworkingoccasionally,
  Occupation,
  Poultryincome,
  Hireland,
  Chickenbreed,
  Chickenbreedname,
  Keepchicken,
  HouseKind,
  Housenumber,
  Canvas,
  Automaticfeed:Groupcooperative,
  Changefeed,
  APfeed,
  OtherAP,
  APfrequency,
  APsource,
  Industrialfeedsource,
  Rawmaterialsfeedsource,
  Otherrawmaterialsfeedsource,
  Feedcost,
  DOCsource,
  OtherDOCsource,
  Mainoutlet,
  Othermainoutlet,
  Secondoutlet,
  Trademark,
  Otherproductfarm,
  Manure,
  Othermanure,
  Manuretreatment:Backyardflock,
  Contactotherpoultry,
  Disinfectionprotocol:Disinfectiontime,
  Disinfectionfrequency,
  Otherdisinfectionfrequency,
  Downingtime:Removedeadchicken,
  Deadchicken,
  Otherdeadchicken,
  Pestcontrol:Peopleenterthefarm,
  Biosecurityprotocol,
  Mortalityrate,
  Vaccination,
  Vaccinationprotocol:Vaccinationadvices,
  Othervaccinationadvices,
  Deworming:Anticoccidialdrugs,
  Animalsickeness:APABU,
  Record,
  Calculated_occasionalworker,
  Calc_Feedtype,
  Calculated_familynumber,
  Calculated_employeenumber,
  Calc_NumberWorkOcc,
  HouseKind,
  Otherpoultry,
  Floortype.litter,
  AnimalsickenessnoABU:APnoABU,
  Province,
  District,
  Gender,
  Age,
  Education,
  Occupation,
  Productivecrop,
  Productivecroparea,
  Yearfarmingexperience,
  Startworkinfarm,
  Investmentsource,
  Loanduration,
  Loanfinishpay,
  Borrowmoney,
  Borrowmoneypurpose,
)
# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(TypoData, plain.ascii = F, trim.strings = T) %>%  view


##Recode of the variables
CleanTypoData = TypoData %>%
  mutate(Questionnairecode = as.character(Questionnairecode)) %>%
  mutate(
    Ageflock = ifelse(
      Questionnairecode == "S2905CGT1" |
        Questionnairecode == "S3005CDT1" | Questionnairecode == "S0106CDH1"
      |
        Questionnairecode == "S3005CDA3" |
        Questionnairecode == "S2805CGH3" | Questionnairecode == "S2905CGH3"
      |
        Questionnairecode == "S0106CDH4" | Questionnairecode == "S0106CDH2"
      |
        Questionnairecode == "S0106CDH5" |
        Questionnairecode == "S1306THH1" | Questionnairecode == "S1306THH2"
      |
        Questionnairecode == "N3105SST3" |
        Questionnairecode == "N0306SST10" |
        Questionnairecode == "N2106CMQ12",
      "MultipleAge",
      "SingleAge"
    )
  ) %>%
  mutate(Education = as.character(Education)) %>%
  mutate(
    Education = ifelse(
      Education == "illetrate" | Education == "illiterate" |
        Education == "primaryschool",
      "illetrate/primaryschool",
      Education
    )
  ) %>%
  mutate(Occupation = fct_collapse(Occupation, employee = c("manager", "technician/vet", "worker"))) %>%
  mutate(Investmentsource = as.character(Investmentsource)) %>%
  mutate(Investmentsource = ifelse(
    Investmentsource == "",
    "ownmoney",
    ifelse(
      Investmentsource == "bank relative",
      "relative",
      Investmentsource
    )
  )) %>%
  mutate(Borrowmoney = as.character(Borrowmoney)) %>%
  mutate(Borrowmoney = ifelse(Borrowmoney == "", "No", Borrowmoney)) %>%
  
  mutate(Housenumber = as.numeric(Housenumber)) %>%
  mutate(Housenumberquali = as.factor(ifelse(Housenumber < 2, "<2", ">=2"))) %>%
  
  mutate(Calculated_familynumber = as.numeric(Calculated_familynumber)) %>%
  mutate(Calculated_familynumberquali = as.factor(ifelse(Calculated_familynumber <
                                                           1, "No", "Yes"))) %>%
  mutate(Calculated_employeenumber = as.numeric(Calculated_employeenumber)) %>%
  mutate(Calculated_employeenumberquali = as.factor(ifelse(Calculated_employeenumber <
                                                             1, "No", "Yes"))) %>%
  mutate(Calc_NumberWorkOcc = as.numeric(Calc_NumberWorkOcc)) %>%
  mutate(Calc_NumberWorkOccquali = as.factor(ifelse(
    Calc_NumberWorkOcc < 1,
    "0",
    ifelse(Calc_NumberWorkOcc <=
             3, "1-3", ">3")
  ))) %>%
  mutate(Calc_NumberWorkOccquali = as.character(Calc_NumberWorkOccquali)) %>%
  mutate(Workingoccasionally = as.character(Workingoccasionally)) %>%
  mutate(
    Calc_NumberWorkOccquali = ifelse(Workingoccasionally == "no", "0", Calc_NumberWorkOccquali)
  ) %>%
  
  mutate(Calc_NumberWorkOcc = as.character(Calc_NumberWorkOcc)) %>%
  mutate(Calc_NumberWorkOcc = ifelse(Calc_NumberWorkOcc == "", "0", Calc_NumberWorkOcc)) %>%
  
  mutate(Otheranimal = as.character(Otheranimal)) %>%
  mutate(Otheranimalspecie = as.character(Otheranimalspecie)) %>%
  mutate(Otheranimal = ifelse(Otheranimalspecie == "otherpoultry", "No", Otheranimal)) %>%
  mutate(Otheranimal = ifelse(Otheranimalspecie == "Dog", "No", Otheranimal)) %>%
  
  mutate(TypeMainchicken = as.character(TypeMainchicken)) %>%
  mutate(
    TypeMainchicken = ifelse(
      TypeMainchicken == "layinghen eggbreeding"
      |
        TypeMainchicken == "layinghen eggconsumption",
      "layinghen",
      TypeMainchicken
    )
  ) %>%
  mutate(TypeMainchicken = as.factor(TypeMainchicken)) %>%
  mutate(TypeMainchicken = fct_recode(TypeMainchicken, layinghen = "breeder ")) %>%
  
  mutate(Otherchickenstype = as.character(Otherchickenstype)) %>%
  mutate(Otherchickenstype = ifelse(Otherchickenstype == "no", "No", "Yes")) %>%
  mutate(Workingoccasionally = as.character(Workingoccasionally)) %>%
  mutate(Otherworkingoccasionally = as.character(Otherworkingoccasionally)) %>%
  mutate(
    Workingoccasionally = ifelse(
      Otherworkingoccasionally == "Wife and children" |
        Otherworkingoccasionally == "when chicken get sick, hire people for injection",
      "no",
      Workingoccasionally
    )
  ) %>%
  mutate(
    Workingoccasionally = ifelse(
      Otherworkingoccasionally == "Electric maintainace staff" |
        Otherworkingoccasionally == "Cleaning",
      "vaccination disinfection",
      Workingoccasionally
    )
  ) %>%
  mutate(
    Workingoccasionally = ifelse(
      Workingoccasionally == "vaccination  Beak trimming, house sanitation"
      |
        Workingoccasionally == "vaccination disinfection  Beak trimming",
      "vaccination Beak trimmin disinfection",
      Workingoccasionally
    )
  ) %>%
  mutate(Workingoccasionally = fct_collapse(
    Workingoccasionally,
    vaccinationfarmmanagement = c(
      "vaccination Beak trimmin disinfection",
      "vaccination disinfection",
      "disinfection",
      "vaccination  Beak trimming"
    )
  )) %>%
  mutate(Poultryincome = as.character(Poultryincome)) %>%
  mutate(Occupation = as.character(Occupation)) %>%
  mutate(Poultryincome = ifelse(Occupation == "technician/vet", "not concerned", Poultryincome)) %>%
  mutate(Poultryincome = ifelse(Poultryincome == "", "don't know", Poultryincome)) %>%
  mutate(Chickenbreed = as.character(Chickenbreed)) %>%
  mutate(Chickenbreedname = as.character(Chickenbreedname)) %>%
  mutate(Chickenbreed = ifelse(
    Chickenbreedname == "Lai Ai Cap",
    "crossbreed",
    ifelse(Chickenbreedname == "CP", "foreign", Chickenbreed)
  )) %>%
  mutate(Automaticwater = as.character(Automaticwater)) %>%
  mutate(Automaticwater = ifelse(Automaticwater == "", "Yes", Automaticwater)) %>%
  
  mutate(APfeed = as.character(APfeed)) %>%
  mutate(OtherAP = as.character(OtherAP)) %>%
  mutate(APfeed = ifelse(APfeed == "" |
                           APfeed == "other" | APfeed == "NA", "no APfeed", APfeed)) %>% ## OK
  mutate(
    APfeed = ifelse(
      APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "liver detoxify, antifungal"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "liver detoxify"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "Liver detoxify, antibiotic"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "liver detoxify, antibiotic"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "Antibiotic, liver detoxify"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "Antibiotic, renal detoxify"
      |
        APfeed == "probiotic vitamins other" &
        OtherAP == "Antibiotic, liver detoxify"
      |
        APfeed == "probiotic vitamins electrolytes other" &
        OtherAP == "liver detoxify 3 days after antibiotic treatment
                       Vitamin + electrolytes: 3 days per week
                       Probiotic 5 days per time
                       Antibiotic: at arrival for first 5 day, changing weather for 3 days,"
      
      |
        APfeed == "probiotic vitamins herbs electrolytes other" &
        OtherAP == "Liver detoxify, antibiotic",
      "probiotic vitamins electrolytes detoxifying",
      APfeed
    )
  ) %>%
  mutate(
    APfeed = ifelse(
      APfeed == "probiotic vitamins electrolytes"
      | APfeed == "probiotic vitamins electrolytes "
      |
        APfeed == "probiotic vitamins electrolytes other"
      |
        APfeed == "probiotic vitamins herbs electrolytes",
      "probiotic vitamins electrolytes",
      APfeed
    )
  ) %>%
  mutate(
    APfeed = ifelse(
      APfeed == "probiotic vitamins herbs"
      | APfeed == "probiotic vitamins other"
      | APfeed == "probiotic vitamins"
      | APfeed == "vitamins electrolytes"
      | APfeed == "vitamins other"
      | APfeed == "vitamins herbs electrolytes"
      | APfeed == "vitamins electrolytes other",
      "vitamins electrolytes or probiotic",
      APfeed
    )
  ) %>%
  
  mutate(APfrequency = as.character(APfrequency)) %>%
  mutate(APfrequency = ifelse(APfeed == "", "no APfrequency", APfrequency)) %>%
  mutate(APsource = as.character(APsource)) %>%
  
  mutate(APsource = ifelse(
    APsource == "",
    "don't know",
    ifelse(APsource == "other", "drugstore", APsource)
  )) %>%
  mutate(APsource = fct_collapse(
    APsource,
    company = c("chickencompany", "drugcompany", "feedcompany")
  )) %>%
  mutate(Industrialfeedsource = as.character(Industrialfeedsource)) %>%
  mutate(
    Industrialfeedsource = ifelse(
      Industrialfeedsource == "",
      "noIndustrialfeed",
      Industrialfeedsource
    )
  ) %>%
  mutate(
    Industrialfeedsource = ifelse(
      Industrialfeedsource == "drugfeedstore" |
        Industrialfeedsource == "feedagency",
      "store",
      Industrialfeedsource
    )
  ) %>%
  mutate(Rawmaterialsfeedsource = as.character(Rawmaterialsfeedsource)) %>%
  mutate(Otherrawmaterialsfeedsource = as.character(Otherrawmaterialsfeedsource)) %>%
  mutate(
    Rawmaterialsfeedsource = ifelse(
      Otherrawmaterialsfeedsource == "Market" |
        Otherrawmaterialsfeedsource == "Grain factory",
      "feedagency",
      Rawmaterialsfeedsource
    )
  ) %>%
  mutate(
    Rawmaterialsfeedsource = ifelse(
      Otherrawmaterialsfeedsource == "paddy of farm, buying the corn"
      | Rawmaterialsfeedsource == "",
      "noRawmaterial",
      Rawmaterialsfeedsource
    )
  ) %>%
  mutate(Rawmaterialsfeedsource = fct_collapse(
    Rawmaterialsfeedsource,
    store = c("drugfeedstore", "feedagency")
  )) %>%
  mutate(Feedcost = as.character(Feedcost)) %>%
  mutate(Feedcost = ifelse(Feedcost == "", "don't know", Feedcost)) %>%
  
  mutate(DOCsource = as.character(DOCsource)) %>%
  mutate(OtherDOCsource = as.character(OtherDOCsource)) %>%
  mutate(
    DOCsource = ifelse(
      OtherDOCsource == "His mother's farm, His mother have breeder farm and provide DOCs for him" |
        OtherDOCsource == "From the contractual friend" |
        OtherDOCsource == "Ben luc breeding farm" |
        OtherDOCsource == "Breed agency" |
        OtherDOCsource == "From Other farm, they sell 5 day old chicks",
      "localhatc",
      ifelse(
        OtherDOCsource == "Contract company provides 17 week-old chickens for contract farm" |
          OtherDOCsource == "This company have 2 kinds of farm: breeder farm and laying hen farm, Chicken raise in breeder farm until 17week-olds will transfer to laying hen farm" |
          OtherDOCsource == "Buying the chicken 16 weeks age" |
          OtherDOCsource == "Buying the chicken 17 weeks age",
        "chickencompany",
        DOCsource
      )
    )
  ) %>%
  mutate(DOCsource = ifelse(
    DOCsource == "trader" |
      DOCsource == "market",
    "trader/market",
    DOCsource
  )) %>%
  mutate(DOCsource = fct_collapse(DOCsource,
                                  localhatc = c("localhatc", "govhatchery"))) %>%
  mutate(DOCsource = fct_collapse(DOCsource,
                                  store = c("feedagency", "drugstore"))) %>%
  mutate(DOC.pulletsource = DOCsource,
         .keep = "unused",
         .after = "Feedcost") %>%
  
  mutate(Mainoutlet = as.character(Mainoutlet)) %>%
  mutate(Othermainoutlet = as.character(Othermainoutlet)) %>%
  mutate(Mainoutlet = ifelse(
    Mainoutlet == "events",
    "directselling",
    ifelse(
      Othermainoutlet == "Emivest company (this farm is not contract with emivest company)"
      |
        Othermainoutlet == "Contract Company introduce private company come to buy eggs",
      "chickencompany",
      Mainoutlet
    )
  )) %>%
  mutate(Mainoutlet = ifelse(Mainoutlet == "trader", "middleman", Mainoutlet)) %>%
  
  mutate(NewsystemStatus = as.character(NewsystemStatus)) %>%
  mutate(
    Mainoutlet = ifelse(
      Othermainoutlet == "Slaughter house" &
        NewsystemStatus == "intensive company and contract",
      "chickencompany",
      Mainoutlet
    )
  ) %>%
  mutate(
    Mainoutlet = ifelse(
      Mainoutlet == "other" &
        NewsystemStatus == "intensive company and contract",
      "chickencompany",
      Mainoutlet
    )
  ) %>%
  mutate(Mainoutlet = ifelse(Mainoutlet == "other", "middleman", Mainoutlet)) %>%
  
  mutate(Secondoutlet = as.character(Secondoutlet)) %>%
  mutate(
    Secondoutlet = ifelse(
      Secondoutlet == "events" |
        Secondoutlet == "ownconsumption",
      "directselling/ownconsumption",
      Secondoutlet
    )
  ) %>%
  mutate(Secondoutlet = ifelse(Secondoutlet == "other", "middleman/market", Secondoutlet)) %>%
  mutate(
    Secondoutlet = ifelse(
      Secondoutlet == "directselling",
      "directselling/ownconsumption",
      Secondoutlet
    )
  ) %>%
  mutate(
    Secondoutlet = ifelse(
      Secondoutlet == "localmarket" |
        Secondoutlet == "trader",
      "middleman/market",
      Secondoutlet
    )
  ) %>%
  
  mutate(Trademark = as.character(Trademark)) %>%
  mutate(Trademark = ifelse(Trademark == "", "Yes", Trademark)) %>%
  mutate(Otherproductfarm = as.character(Otherproductfarm)) %>%
  mutate(Otherproductfarm = ifelse(Otherproductfarm == "no", "No", "Yes")) %>%
  mutate(Manure = as.character(Manure)) %>%
  mutate(Othermanure = as.character(Othermanure)) %>%
  mutate(
    Manure = ifelse(
      Othermanure == "Ferment, dry and sell manure"
      |
        Othermanure == "Cleaning, washing the chicken house in daily and using the biogas",
      "sell",
      Manure
    )
  ) %>%
  mutate(Manuretreatment = as.character(Manuretreatment)) %>%
  mutate(Manuretreatment = ifelse(Manuretreatment == "", "Yes", Manuretreatment)) %>%
  
  
  mutate(Disinfectiontime = as.character(Disinfectiontime)) %>%
  mutate(
    Disinfectiontime = ifelse(
      Disinfectiontime == "" |
        Disinfectiontime == "other",
      "don't know",
      Disinfectiontime
    )
  ) %>%
  
  mutate(Disinfectiontime = fct_collapse(
    Disinfectiontime,
    duringbatchafterselling = c(
      "duringbatch beforeselling afterselling",
      "duringbatch afterselling beforeselling",
      "beforeselling afterselling",
      "duringbatch afterselling"
    )
  )) %>%
  mutate(Disinfectiontime = fct_collapse(
    Disinfectiontime,
    duringbatch = c("duringbatch beforeselling", "duringbatch")
  )) %>%
  mutate(Otherdisinfectionfrequency = as.character(Otherdisinfectionfrequency)) %>%
  mutate(Disinfectionfrequency = as.character(Disinfectionfrequency)) %>%
  mutate(Disinfectionfrequency = ifelse(
    Disinfectionfrequency == "",
    "No disinfection",
    ifelse(
      Otherdisinfectionfrequency == "3 to 4  times / year",
      "4",
      ifelse(
        Otherdisinfectionfrequency == "Disinfect when chicken get sick",
        "1",
        ifelse(
          Otherdisinfectionfrequency == "2 times per year during the district campaign",
          "2",
          Disinfectionfrequency
        )
      )
    )
  )) %>%
  mutate(Disinfectionfrequency = fct_collapse(Disinfectionfrequency, "<4" =
                                                c("1", "2", "3"))) %>%
  
  mutate(Downingtime = as.character(Downingtime)) %>%
  mutate(Downingtime = ifelse(Downingtime == "", "0", Downingtime)) %>%
  mutate(Downingtime = ifelse(Downingtime == "0" |
                                Downingtime == "1", "<2", Downingtime)) %>%
  mutate(Isolatetedarea = as.character(Isolatetedarea)) %>%
  mutate(Isolatetedarea = ifelse(Isolatetedarea == "", "Yes", Isolatetedarea)) %>%
  mutate(Removedeadchicken = as.character(Removedeadchicken)) %>%
  mutate(
    Removedeadchicken = ifelse(
      Removedeadchicken == "" |
        Removedeadchicken == "never",
      "morethan2days",
      Removedeadchicken
    )
  ) %>%
  mutate(Deadchicken = as.character(Deadchicken)) %>%
  mutate(Deadchicken = ifelse(Deadchicken == "", "burry", Deadchicken)) %>%
  mutate(Otherdeadchicken = as.character(Otherdeadchicken)) %>%
  mutate(
    Deadchicken = ifelse(
      Otherdeadchicken == "Selling the died one in case this died chicken was not given by medication, whereas burried them"
      |
        Otherdeadchicken == "Burn, burry, feed Crocodile",
      "burry",
      ifelse(
        Otherdeadchicken == "Collect by public trash bin"
        |
          Otherdeadchicken == "thrown in to the public trash bin"
        |
          Otherdeadchicken == "Through away ( garbic bin)",
        "collect",
        ifelse(
          Otherdeadchicken == "both burn, burry and give for other animals"
          |
            Otherdeadchicken == "Burn then give for fish"
          |
            Otherdeadchicken == "Sell to Crocodile farm",
          "giveanimal",
          ifelse(
            Otherdeadchicken == "Collect to the freezer then burn"
            |
              Otherdeadchicken == "Using the biogas",
            "burn",
            Deadchicken
          )
        )
      )
    )
  ) %>%
  
  mutate(Distancefarm = as.character(Distancefarm)) %>%
  mutate(Distancefarm = ifelse(Distancefarm == "", "<500m", Distancefarm)) %>%
  mutate(Peopleenterthefarm = as.character(Peopleenterthefarm)) %>%
  mutate(Peopleenterthefarm = fct_collapse(
    Peopleenterthefarm,
    onlystaffandvet = c("farmstaffs", "vet", "farmstaffs vet")
  )) %>%
  mutate(Peopleenterthefarm = as.character(Peopleenterthefarm)) %>%
  mutate(
    Peopleenterthefarm = ifelse(
      Peopleenterthefarm == "onlystaffandvet",
      "onlystaffandvet",
      "differentvisitors"
    )
  ) %>%
  
  mutate(Biosecurityprotocol = as.character(Biosecurityprotocol)) %>%
  mutate(Biosecurityprotocol = ifelse(Biosecurityprotocol == "", "No", Biosecurityprotocol)) %>%
  
  mutate(Estimatemortality = as.character(Estimatemortality)) %>%
  mutate(Mortalityrate = as.numeric(Mortalityrate)) %>%
  mutate(Mortalityratequali = as.factor(ifelse(
    Mortalityrate <= 5,
    "<=5",
    ifelse(Mortalityrate <= 10, "5-10", ">10")
  ))) %>%
  mutate(Mortalityratequali = as.character(Mortalityratequali)) %>%
  mutate(
    Mortalityratequali = ifelse(
      Estimatemortality == "" |
        Estimatemortality == "No",
      "don't know",
      Mortalityratequali
    )
  ) %>%
  
  mutate(Vaccinationprotocol = as.character(Vaccinationprotocol)) %>%
  mutate(Vaccination = as.character(Vaccination)) %>%
  mutate(
    Vaccinationprotocol = ifelse(
      Vaccination == "Yes" & Vaccinationprotocol == ""
      |
        Vaccinationprotocol == "No",
      "Yes without protocol",
      ifelse(
        Vaccinationprotocol == "Yes",
        "Yes with protocol",
        ifelse(
          Vaccination == "No" &
            Vaccinationprotocol == "",
          "no vaccination",
          Vaccinationprotocol
        )
      )
    )
  ) %>%
  mutate(Vaccinationadvices = as.character(Vaccinationadvices)) %>%
  mutate(Vaccinationadvices = ifelse(Vaccinationadvices == "", "no vaccination", Vaccinationadvices)) %>%
  mutate(Vaccinationadvices = fct_collapse(
    Vaccinationadvices,
    company = c("drugcompany", "feedcompany", "breedingcom")
  )) %>%
  mutate(Othervaccinationadvices = as.character(Othervaccinationadvices)) %>%
  mutate(Vaccinationadvices = as.character(Vaccinationadvices)) %>%
  mutate(
    Vaccinationadvices = ifelse(
      Othervaccinationadvices == "Brother",
      "ownexperience",
      ifelse(
        Othervaccinationadvices == "Workshop form the local goverment and drug company"
        |
          Othervaccinationadvices == "Lifsap program"
        |
          Othervaccinationadvices == "Lifsap program (stop from 2018) then have their own experience"
        |
          Othervaccinationadvices == "Lecturer from college"
        |
          Othervaccinationadvices == "local hatchery",
        "ownexperience",
        ifelse(
          Othervaccinationadvices == "Veterinarians at farm",
          "company",
          Vaccinationadvices
        )
      )
    )
  ) %>%
  unite(
    Vaccination,
    c(Vaccinationprotocol, Vaccinationadvices),
    sep = " ",
    remove = F
  ) %>%
  mutate(
    Vaccination = ifelse(
      Vaccination == "Yes without protocol company" |
        Vaccination == "Yes without protocol drugstore" |
        Vaccination == "Yes without protocol localvet"
      |
        Vaccination == "Yes without protocol ownexperience",
      "Yes without protocol",
      Vaccination
    )
  ) %>%
  mutate(
    Vaccination = ifelse(
      Vaccination == "no vaccination no vaccination",
      "no vaccination",
      Vaccination
    )
  ) %>%
  
  mutate(Deworming = as.character(Deworming)) %>%
  mutate(Deworming = ifelse(Deworming == "", "No", Deworming)) %>%
  mutate(Dewormingfrequency = as.character(Dewormingfrequency)) %>%
  mutate(
    Dewormingfrequency = ifelse(
      Dewormingfrequency == ""
      |
        Dewormingfrequency == "Only when the herd have worm, diagnosis by necropsy of the weak chicken",
      "No deworm",
      Dewormingfrequency
    )
  ) %>%
  mutate(
    Dewormingfrequency = ifelse(
      Dewormingfrequency == "4 months/time"
      | Dewormingfrequency == "1",
      "1",
      ifelse(
        Dewormingfrequency == "2",
        "2",
        ifelse(Dewormingfrequency == "No deworm", "No deworm", ">2")
      )
    )
  ) %>%
  
  mutate(Anticoccidialdrugs = as.character(Anticoccidialdrugs)) %>%
  mutate(Anticoccidialdrugs = ifelse(Anticoccidialdrugs == "", "No", Anticoccidialdrugs)) %>%
  mutate(Animalsickeness = as.character(Animalsickeness)) %>%
  mutate(Animalsickeness = ifelse(Animalsickeness == "", "don't know", Animalsickeness)) %>%
  mutate(Record = as.character(Record)) %>%
  mutate(Record = ifelse(Record == "", "No", Record)) %>%
  mutate(Floortype.litter = as.character(Floortype.litter)) %>%
  mutate(Floortype.litter = ifelse(Floortype.litter == "", "freerange", Floortype.litter)) %>%
  
  
  
  unite(APsickness,
        c(APABU, APnoABU),
        sep = " ",
        remove = F) %>%
  mutate(APsickness = as.character(APsickness)) %>%
  mutate(
    APsickness = ifelse(
      APsickness == "probiotic vitamins electrolytes " |
        APsickness == " probiotic vitamins electrolytes" |
        APsickness == "probiotic vitamins electrolytes other " |
        APsickness == "probiotic vitamins herbs electrolytes ",
      "probiotic vitamins electrolytes",
      APsickness
    )
  ) %>%
  mutate(
    APsickness = ifelse(
      APsickness == " " |
        APsickness == "other " |
        APsickness == "herbs ",
      "no APsickness",
      APsickness
    )
  ) %>%
  mutate(
    APsickness = ifelse(
      APsickness == "probiotic vitamins " |
        APsickness == "vitamins electrolytes " |
        APsickness == "probiotic vitamins other " |
        APsickness == "vitamins herbs electrolytes ",
      "vitamins electrolytes or probiotic",
      APsickness
    )
  ) %>%
  mutate(
    APsickness = ifelse(
      APsickness == "probiotic " |
        APsickness == "electrolytes " |
        APsickness == "probiotic herbs " |
        APsickness == "vitamins " |
        APsickness == "vitamins other ",
      "vitamins or electrolytes or probiotic",
      APsickness
    )
  ) %>%
  mutate(
    Indusfeed = ifelse(
      Calc_Feedtype == "indus" |
        Calc_Feedtype == "indus rawmaterial" |
        Calc_Feedtype == "indus rawmaterial scavenging" |
        Calc_Feedtype == "indus scavenging" |
        Calc_Feedtype == "indus other" |
        Calc_Feedtype == "indus rawmaterial other" |
        Calc_Feedtype == "indus rawmaterial scavenging other" |
        Calc_Feedtype == "indus scavenging other",
      "Yes",
      "No"
    )
  ) %>%
  mutate(
    Rawfeed = ifelse(
      Calc_Feedtype == "indus rawmaterial" |
        Calc_Feedtype == "indus rawmaterial scavenging" |
        Calc_Feedtype == "indus rawmaterial other" |
        Calc_Feedtype == "rawmaterial other" |
        Calc_Feedtype == "rawmaterial scavenging other" |
        Calc_Feedtype == "indus rawmaterial scavenging other",
      "Yes",
      "No"
    )
  ) %>%
  mutate(
    Scavengingfeed = ifelse(
      Calc_Feedtype == "indus rawmaterial scavenging" |
        Calc_Feedtype == "indus scavenging" |
        Calc_Feedtype == "rawmaterial scavenging other" |
        Calc_Feedtype == "indus rawmaterial scavenging other" |
        Calc_Feedtype == "indus scavenging other",
      "Yes",
      "No"
    )
  ) %>%
  mutate(
    Otherfeed = ifelse(
      Calc_Feedtype == "indus other" |
        Calc_Feedtype == "indus rawmaterial other" |
        Calc_Feedtype == "rawmaterial other" |
        Calc_Feedtype == "indus rawmaterial scavenging other" |
        Calc_Feedtype == "indus scavenging other" |
        Calc_Feedtype == "indus scavenging other",
      "Yes",
      "No"
    )
  ) %>%
  mutate(Cerealfeed = Rawfeed) %>%
  
  mutate(Manure = as.character(Manure)) %>%
  mutate(Manuretreatment = as.character(Manuretreatment)) %>%
  mutate(Manure = ifelse(Manuretreatment == "Yes" &
                           Manure == "crop", "crop with trt", Manure)) %>%
  mutate(Manure = ifelse(
    Manuretreatment == "No" &
      Manure == "crop",
    "crop without trt",
    Manure
  )) %>%
  mutate(Manure = ifelse(Manuretreatment == "Yes" &
                           Manure == "sell", "sell with trt", Manure)) %>%
  mutate(Manure = ifelse(
    Manuretreatment == "No" &
      Manure == "sell",
    "sell without trt",
    Manure
  )) %>%
  
  mutate(Contactotherpoultry = as.character(Contactotherpoultry)) %>%
  mutate(Contactotherpoultry = ifelse(Contactotherpoultry == "", "No", Contactotherpoultry)) %>%
  mutate(Cerealfeedsource = Rawmaterialsfeedsource, .keep = "unused") %>%
  mutate(
    Cerealfeedsource = ifelse(
      Cerealfeedsource == "noRawmaterial",
      "noCerealfeed",
      Cerealfeedsource
    )
  ) %>%
  mutate(FarmType = NewsystemStatus) %>%
  mutate(Worker.family = Calculated_familynumberquali) %>%
  mutate(Worker.employee = Calculated_employeenumberquali) %>%
  mutate(Worker.occasionally = Workingoccasionally) %>%
  mutate(Housing = Keepchicken) %>%
  mutate(MortalityRate = Mortalityratequali) %>%
  mutate(Housenumber = as.character(Housenumber)) %>%
  mutate(Worker.occasionally = Calculated_occasionalworker)

# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(CleanTypoData,plain.ascii = F,trim.strings = T) %>%  view


########################################
#### MCA with factoshinny and factomineR
########################################

###Selection of the variables to conduct MCA/HCA
SecMCA2TypoData = CleanTypoData %>% select(
  Calculated_number,
  Province,
  Distancefarm,
  Gender,
  Age,
  Education,
  Yearfarmingexperience,
  Startworkinfarm,
  FarmType,
  TypeMainchicken,
  APfeed,
  APsickness,
  Poultryincome,
  Feedcost,
  Worker.employee,
  Mainactivity,
  Borrowmoney,
  Hireland,
  Productivecrop,
  Investmentsource,
  Worker.occasionally,
  Worker.family,
  Otheranimal,
  Contactotherpoultry,
  Record,
  DOC.pulletsource,
  Mainoutlet,
  Chickenbreed,
  Housing,
  Ageflock,
  Automaticwater,
  Changefeed,
  Scavengingfeed,
  Otherfeed,
  Cerealfeed,
  Deworming,
  Downingtime,
  Peopleenterthefarm,
  Isolatetedarea,
  Biosecurityprotocol,
  Pestcontrol,
  Anticoccidialdrugs,
  MortalityRate
) %>% mutate_if(is.character, as.factor)

# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(SecMCA2TypoData,plain.ascii = F,trim.strings = T) %>%  view

### MCA/HCA using Factoshiny package

test2 <- SecMCA2TypoData[, c(1:43)]

# uncomment this line and execute to launch an interactive MCA shiny app on your browser
#MCAshiny(test2)

##### MCA/HCA with FactoMineR
# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
# dfSummary(SecMCA2TypoData,
#           plain.ascii = F,
#           trim.strings = T) %>%  view

res.mca <-
  MCA(
    SecMCA2TypoData,
    quali.sup = c(2:22),
    quanti.sup = 1,
    ncp = 6
  ) #run MCA with PC=6
res.mca$eig # show eigenvalue and variance

res.hcpc2 <-
  HCPC(res.mca, consol = T, method = "ward") #select number of clusters on the graph
res.hcpc2$desc.var # description of each clusters
head(res.hcpc2$data.clust, 10) #show the results for the ten first individuals

mca2 <-
  res.hcpc2$data.clust ### creation of the new dataset with ClusterTypo
summary(mca2$clust) # show the number of individuals in each cluster

fviz_cluster(res.hcpc2, ellipse.type = "convex", ggtheme = theme_bw()) # Clusterplot

fviz_dend(res.hcpc2) ### Cluster dendrogram


#### Rename the variable ClusterTypo
ClusterTypo = mca2$clust
mca3 = mca2 %>%
  mutate(ClusterTypo = as.character(ClusterTypo)) %>%
  mutate(ClusterTypo = ifelse(ClusterTypo == "1", "A",
                              ifelse(ClusterTypo == "2", "B", "C"))) %>%
  mutate(ClusterTypo = as.factor(ClusterTypo)) %>%
  mutate(ClusterTypo = as.character(ClusterTypo))
ClusterTypo = mca3$ClusterTypo

# We have created a new variable, ClusterTypo, that represents the typology of farming practices

#### 2) Realization of the typology of antibiotic use patterns using MCA and HCA

#### Now, we want to create the typology of AB pattern from the same dataset


Data = read.csv(
  "CleandataR2-1.csv",
  sep = ";",
  h = T,
  stringsAsFactors = T
)
Data %>% glimpse

## Selection of the variables related to AB in the whole data set and cleaning


ABData = Data %>% select(
  Province,
  NewsystemStatus,
  ABknowledge:Otheranimalsickness,
  ABUreason:ABUreason.growthpromote,
  ABfeed:FormulationABfeed.formulation6,
  APfeed.AB ,
  OtherAP:OtherAPfrequency,
  TrainingABU:Comment
) %>% filter(ABgiven == "Yes") %>%
  mutate(OtherAPfrequency = as.character(OtherAPfrequency)) %>%
  mutate(APfrequency.AB = OtherAPfrequency, .after = "OtherAPfrequency") %>%
  mutate(APfrequency.AB = ifelse(APfeed.AB == "No", "NA", APfrequency.AB)) %>%
  mutate(FarmType = NewsystemStatus) %>%
  mutate(
    OtherAP = NULL,
    APfrequency = NULL,
    OtherAPfrequency = NULL,
    Recordinformation = NULL,
    Recordinformation.vaccination = NULL,
    Recordinformation.numberchicken = NULL,
    Recordinformation.deathchicken = NULL,
    Recordinformation.additionalchic
    = NULL,
    Recordinformation.diseases = NULL,
    Recordinformation.temperature = NULL,
    Recordinformation.feed = NULL,
    Recordinformation.other = NULL,
    Recordinformation.NA = NULL,
    Otherrecordinformation = NULL,
    Comment = NULL
  )

# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(ABData, plain.ascii = F, trim.strings = T) %>%  view

##Recoding of the variables and creation of a new dataset, CleanABData2

CleanABData2 = ABData %>%
  mutate(Calculate_ABUreason = as.character(ABUreason)) %>%
  mutate(
    Calculate_ABUreason = ifelse(
      Calculate_ABUreason == "prevention",
      "chickensicknes prevention",
      Calculate_ABUreason
    )
  ) %>%
  mutate(ProtocoleABUprevention = as.character(ProtocoleABUprevention)) %>%
  mutate(Calculate_ABUreason = as.character(Calculate_ABUreason)) %>%
  mutate(
    ProtocoleABUprevention = ifelse(
      Calculate_ABUreason == "chickensicknes",
      "No",
      ProtocoleABUprevention
    )
  ) %>%
  mutate(ABUadvices = as.character(ABUadvices)) %>%
  mutate(ABUadvices = fct_recode(ABUadvices, ownexperience = "other")) %>%
  mutate(ABUadvices = fct_collapse(
    ABUadvices,
    company = c("chickencompany", "feedcompany", "breedingcom", "drugcompany")
  )) %>%
  mutate(ABUadvices = fct_recode(ABUadvices, ownexperience = "N")) %>%
  mutate(ABUwayofadvices = as.character(ABUwayofadvices)) %>%
  mutate(ABUadvices = as.character(ABUadvices)) %>%
  mutate(
    ABUwayofadvices = ifelse(
      ABUadvices == "ownexperience" |
        ABUadvices == "N"
      ,
      "no professionnal",
      ABUwayofadvices
    )
  ) %>%
  mutate(Diagnostictest = as.character(Diagnostictest)) %>%
  mutate(
    Diagnostictest = ifelse(
      ABUwayofadvices == "phone" |
        ABUwayofadvices == "no professionnal" |
        ABUwayofadvices == "contactatvetsh",
      "no test",
      Diagnostictest
    )
  ) %>%
  mutate(Diagnostictest = ifelse(Diagnostictest == "other", "no test", Diagnostictest)) %>%
  
  mutate(SecondABUadvices = as.character(SecondABUadvices)) %>%
  mutate(OthersecondABUadvices = as.character(OthersecondABUadvices)) %>%
  mutate(
    SecondABUadvices = ifelse(
      OthersecondABUadvices == "Drug workshop" |
        OthersecondABUadvices == "Lecturer from Nong Lam university,",
      "ownexperience",
      SecondABUadvices
    )
  ) %>%
  mutate(
    SecondABUadvices = ifelse(
      OthersecondABUadvices == "Higher vet position",
      "localvet",
      SecondABUadvices
    )
  ) %>%
  mutate(
    SecondABUadvices = ifelse(
      OthersecondABUadvices == "many sources",
      "otherfarmers",
      SecondABUadvices
    )
  ) %>%
  mutate(SecondABUadvices = fct_collapse(
    SecondABUadvices,
    company = c("chickencompany", "feedcompany", "drugcompany")
  )) %>%
  mutate(SecondABUadvices = as.character(SecondABUadvices)) %>%
  mutate(OthersecondABUadvices = as.character(OthersecondABUadvices)) %>%
  mutate(
    SecondABUadvices = ifelse(
      SecondABUadvices == "internet",
      "ownexperience",
      SecondABUadvices
    )
  ) %>%
  mutate(SecondABUadvices = as.factor(SecondABUadvices)) %>%
  mutate(SecondABUadvices = fct_recode(SecondABUadvices, no = "No")) %>%
  
  mutate(ABsource = as.character(ABsource)) %>%
  mutate(OtherABsource = as.character(OtherABsource)) %>%
  mutate(ABsource = ifelse(
    OtherABsource == "Pharmaceutical company",
    "drugcompany",
    ABsource
  )) %>%
  mutate(
    ABsource = ifelse(
      OtherABsource == "Ask feed agency for buying medicine",
      "drugstore",
      ABsource
    )
  ) %>%
  mutate(ABsource = fct_recode(ABsource, drugstore = "localvet"))  %>%
  mutate(ABsource = fct_recode(ABsource, retailer = "drugstore")) %>%
  mutate(ABsource = fct_collapse(ABsource,
                                 company = c("chickencompany", "drugcompany"))) %>%
  mutate(ABdosage = fct_recode(ABdosage, givemore = "drugseller")) %>%
  mutate(ABdosage = fct_recode(ABdosage, giveless = "writtenonprodu")) %>%
  mutate(ABdosage = fct_recode(ABdosage, followadvice = "vetinstruct")) %>%
  mutate(ABdosage = as.character(ABdosage)) %>%
  mutate(OtherABdosage = as.character(OtherABdosage)) %>%
  mutate(ABdosage = fct_recode(ABdosage, givemore = "other")) %>%
  
  mutate(ABtrtlength = as.character(ABtrtlength)) %>%
  mutate(OtherABtrtlength = as.character(OtherABtrtlength)) %>%
  mutate(ABtrtlength = ifelse(OtherABtrtlength == "Own experience", "doubletime", ABtrtlength)) %>%
  mutate(
    ABtrtlength = ifelse(
      OtherABtrtlength == "Following own experience" |
        ABtrtlength == "",
      "followadvice",
      ABtrtlength
    )
  ) %>%
  mutate(ABtrtlength = ifelse(
    OtherABtrtlength == "Owner expericence",
    "doubletime",
    ABtrtlength
  )) %>%
  mutate(
    ABtrtlength = ifelse(
      OtherABtrtlength == "1 to 2 days" |
        OtherABtrtlength == "2-3 days" |
        OtherABtrtlength == "Always uses 2 to 3 days",
      "lesstime",
      ABtrtlength
    )
  ) %>%
  mutate(
    ABtrtlength = ifelse(
      OtherABtrtlength == "3 days" |
        OtherABtrtlength == "3 days continuously for all types" |
        OtherABtrtlength == "5 days" |
        OtherABtrtlength == "3-5days" |
        OtherABtrtlength == "3 days for the brooding preriod, 3 days for treatment" |
        OtherABtrtlength == "Own exprience during 3 days" |
        OtherABtrtlength == "Respiratory disease 3-5 days, Intestinal disease 4 days" |
        OtherABtrtlength == "Following drug instruction",
      "followadvice",
      ABtrtlength
    )
  ) %>%
  mutate(
    ABtrtlength = ifelse(
      OtherABtrtlength == "Own experience: using for 3-7 days" |
        OtherABtrtlength == "7 days",
      "moretime",
      ABtrtlength
    )
  ) %>%
  mutate(ABtrtlength = as.factor(ABtrtlength)) %>%
  mutate(ABtrtlength = fct_recode(ABtrtlength, moretime = "doubletime")) %>%
  mutate(ABtrtlength = fct_recode(ABtrtlength, moretime = "finishdrug")) %>%
  
  mutate(Administerallchicken = as.character(Administerallchicken)) %>%
  mutate(Dependadministerallchicken = as.character(Dependadministerallchicken)) %>%
  mutate(Administerallchicken = fct_recode(Administerallchicken, No = "depend")) %>%
  mutate(Measuresnotcured = fct_recode(Measuresnotcured, changeAB = "doubledose")) %>%
  mutate(Measuresnotcured = as.character(Measuresnotcured)) %>%
  mutate(Othermeasuresnotcured = as.character(Othermeasuresnotcured)) %>%
  mutate(
    Measuresnotcured = ifelse(
      Othermeasuresnotcured == "Sometimes increase dose or change drug, culling in most of serious cases" |
        Othermeasuresnotcured == "If they did not recover, he injected the antibiotic for 2 days and add more bromhexin",
      "changeAB",
      Measuresnotcured
    )
  ) %>%
  
  mutate(
    Measuresnotcured = ifelse(
      Othermeasuresnotcured == "Don't have case of not recovering" |
        Othermeasuresnotcured == "Don't have disease" |
        Othermeasuresnotcured == "Until the chicken recover" |
        Othermeasuresnotcured == "Don't have case of not recovering" |
        Measuresnotcured == "" |
        Othermeasuresnotcured == "Don't have case of not recovering",
      "don't have the case",
      Measuresnotcured
    )
  ) %>%
  
  
  mutate(
    Measuresnotcured = ifelse(
      Othermeasuresnotcured == "Necropsy" |
        Othermeasuresnotcured == "Necropsy, ask for advice" |
        Othermeasuresnotcured == "The company's veterinarian decides" |
        Othermeasuresnotcured == "Increase to 5 days and ask for advice from vet from contract company (PCR, Ab sensitivity test)",
      "askadvice",
      Measuresnotcured
    )
  ) %>%
  mutate(
    Measuresnotcured = ifelse(
      Othermeasuresnotcured == "If the treatment is not successful, the chicken will die",
      "cull",
      Measuresnotcured
    )
  ) %>%
  mutate(Measuresnotcured = as.factor(Measuresnotcured)) %>%
  
  mutate(SellafterABU = as.character(SellafterABU)) %>%
  mutate(SellafterABU = ifelse(SellafterABU == "", "don't know", SellafterABU)) %>%
  mutate(SellafterABU = as.factor(SellafterABU)) %>%
  mutate(SellafterABU = fct_recode(SellafterABU, vetproductinstr = "productinstr")) %>%
  mutate(SellafterABU = fct_recode(SellafterABU, vetproductinstr = "vetinstr")) %>%
  mutate(SellafterABU = fct_recode(SellafterABU, ownexperience = "directafter")) %>%
  mutate(OthersellafterABU = as.character(OthersellafterABU)) %>%
  mutate(SellafterABU = as.character(SellafterABU)) %>%
  mutate(
    SellafterABU = ifelse(
      OthersellafterABU == "1 month" |
        OthersellafterABU == "15 days",
      "ownexperience",
      SellafterABU
    )
  ) %>%
  mutate(
    SellafterABU = ifelse(
      OthersellafterABU == "No" |
        OthersellafterABU == "Only eat healthy one",
      "don't know",
      SellafterABU
    )
  ) %>%
  mutate(
    SellafterABU = ifelse(
      OthersellafterABU == "No Ab use for laying hens" |
        OthersellafterABU == "Prevent at one month old and stop using antibiotic" |
        OthersellafterABU == "Don't use antibiotic in finishing period" |
        OthersellafterABU == "do not use antibiotics for 2 months before selling chickens" |
        OthersellafterABU == "do not use antibiotics for several months before selling chickens",
      "ownexperience",
      SellafterABU
    )
  ) %>%
  mutate(Leftover = fct_recode(Leftover, throwaway = "other")) %>%
  mutate(ABcost = as.character(ABcost)) %>%
  mutate(ABcost = ifelse(ABcost == "", "don't know", ABcost)) %>%
  mutate(ABcost = ifelse(ABcost == "other", "<25", ABcost)) %>%
  mutate(ABcost = ifelse(ABcost == "1", "<25", ABcost)) %>%
  mutate(ABcost = ifelse(ABcost == ">75", ">50", ABcost)) %>%
  mutate(ABcost = ifelse(ABcost == "51-75", ">50", ABcost)) %>%
  mutate(ABcost = as.factor(ABcost)) %>%
  mutate(Drugcost = as.character(Drugcost)) %>%
  mutate(Drugcost = ifelse(Drugcost == "<1%", "1-3%", Drugcost)) %>%
  mutate(Drugcost = ifelse(Drugcost == "1-3%", "<3%", Drugcost)) %>%
  mutate(Drugcost = ifelse(Drugcost == "", "don't know", Drugcost)) %>%
  mutate(Drugcost = as.factor(Drugcost)) %>%
  mutate(Trainingsource = as.character(Trainingsource)) %>%
  mutate(Othertrainingsource = as.character(Othertrainingsource)) %>%
  mutate(
    Othertrainingsource = fct_recode(Othertrainingsource, vetstation = "local veterinary department")
  ) %>%
  mutate(
    Othertrainingsource = fct_recode(Othertrainingsource, extcenter = "Extensional province: 1 per year (1month), study for 3 years, Topics: Technique, disease prevention, biosecurity, Give certification, \nCommune: extensional organise 1 time per year (1 day)")
  ) %>%
  mutate(Trainingsource = ifelse(
    Othertrainingsource == "vetstation",
    "vetstation",
    Trainingsource
  )) %>%
  mutate(Trainingsource = ifelse((
    grepl("other", Trainingsource) &
      grepl("extcenter", Othertrainingsource)
  ),
  paste(str_remove(Trainingsource, "other"), Othertrainingsource),
  Trainingsource
  )) %>%
  mutate(
    Trainingsource = ifelse(
      Othertrainingsource == "Lecturer from college",
      "public",
      Trainingsource
    )
  ) %>%
  mutate(Trainingsource = fct_collapse(
    Trainingsource,
    company = c(
      "chickencompany",
      "drugcompany chickencompany",
      "drugcompany",
      "drugcompany feedcompany",
      "feedcompany",
      "drugcompany feedcompany chickencompany"
    )
  )) %>%
  mutate(Trainingsource = fct_recode(Trainingsource, extcenter = "extcenter  extcenter")) %>%
  mutate(Trainingsource = fct_collapse(Trainingsource,
                                       public = c("extcenter", "vetstation"))) %>%
  mutate(Trainingsource = fct_collapse(
    Trainingsource,
    both = c(
      "subDAH drugcompany",
      "vetstation drugcompany",
      "vetstation drugcompany feedcompany",
      "subDAH drugcompany feedcompany",
      "vetstation drugcompany chickencompany",
      "vetstation drugcompany feedcompany chickencompany"
    )
  )) %>%
  mutate(Traininglasttime = as.character(Traininglasttime)) %>%
  mutate(TrainingABU = as.character(TrainingABU)) %>%
  mutate(Traininglasttime = ifelse(TrainingABU == "No", "never", Traininglasttime)) %>%
  mutate(
    Traininglasttime = ifelse(
      Traininglasttime == "3m" |
        Traininglasttime == "6m",
      "<6months",
      Traininglasttime
    )
  ) %>%
  mutate(
    Traininglasttime = ifelse(
      Traininglasttime == "1y" |
        Traininglasttime == "2y",
      ">6months",
      Traininglasttime
    )
  ) %>%
  mutate(Traininglasttime = as.factor(Traininglasttime)) %>%
  mutate(Recordinformation.treatment = as.character(Recordinformation.treatment)) %>%
  mutate(Record = as.character(Record)) %>%
  mutate(Recordinformation.treatment = ifelse(Record == "No", "No", Recordinformation.treatment)) %>%
  mutate(Recordtreatment.nameofproduct = as.character(Recordtreatment.nameofproduct)) %>%
  mutate(Recordtreatment.dose = as.character(Recordtreatment.dose)) %>%
  mutate(Recordtreatment.numberchickent = as.character(Recordtreatment.numberchickent)) %>%
  mutate(Recordtreatment.lengthoftreatm = as.character(Recordtreatment.lengthoftreatm)) %>%
  mutate(
    Recordtreatment.nameofproduct = ifelse(
      Recordinformation.treatment == "No",
      "No",
      Recordtreatment.nameofproduct
    )
  ) %>%
  mutate(
    Recordtreatment.dose = ifelse(
      Recordinformation.treatment == "No",
      "No",
      Recordtreatment.dose
    )
  ) %>%
  mutate(
    Recordtreatment.numberchickent = ifelse(
      Recordinformation.treatment == "No",
      "No",
      Recordtreatment.numberchickent
    )
  ) %>%
  mutate(
    Recordtreatment.lengthoftreatm = ifelse(
      Recordinformation.treatment == "No",
      "No",
      Recordtreatment.lengthoftreatm
    )
  ) %>%
  mutate(ABUreason = as.factor(ABUreason)) %>%
  mutate(ProtocoleABUprevention = as.factor(ProtocoleABUprevention)) %>%
  mutate(ABUadvices = as.factor(ABUadvices)) %>%
  mutate(SecondABUadvices = as.factor(SecondABUadvices)) %>%
  mutate(ABUwayofadvices = as.factor(ABUwayofadvices)) %>%
  mutate(Diagnostictest = as.factor(Diagnostictest)) %>%
  mutate(SellafterABU = as.factor(SellafterABU)) %>%
  mutate(TrainingABU = as.factor(TrainingABU)) %>%
  mutate(Traininglasttime = as.factor(Traininglasttime)) %>%
  mutate(Recordinformation.treatment = as.factor(Recordinformation.treatment))

def = CleanABData2$ABdefinition
tmp = data.frame(test = def)
tmp$val = ifelse((
  grepl("Prevent", def, ignore.case = T) |
    grepl("treat", def, ignore.case = T)
) &
  grepl("bacter", def, ignore.case = T),
"correct",
ifelse((
  grepl("Prevent", def, ignore.case = T) |
    grepl("treat", def, ignore.case = T)
) &
  grepl("disease", def, ignore.case = T),
"partially correct",
"incorrect"
)
)
tmp$val %>% as.factor() %>% summary
tmp %>% filter(val == "correct")
CleanABData2$ABdefinitionSimple = tmp$val

CleanABData2 = CleanABData2 %>%
  mutate(ABdefinitionSimple = as.character(ABdefinitionSimple)) %>%
  mutate(ABdefinition = as.character(ABdefinition)) %>%
  mutate(
    ABdefinitionSimple = ifelse(
      ABdefinition == "Treat virus and bacterial disease, prevent disease",
      "partially correct",
      ABdefinitionSimple
    )
  ) %>%
  mutate(ABdefinitionSimple = as.factor(ABdefinitionSimple)) %>%
  
  mutate(FarmType = NewsystemStatus) %>%
  mutate(Withdrawaltime = SellafterABU) %>%
  mutate(ABuse = Calculate_ABUreason)

# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
# dfSummary(CleanABData2,
#           plain.ascii = F,
#           trim.strings = T) %>%  view


##Selection of the variables to conduct the MCA, include also the variable Province
#And add ClusterTypo
MCAABData3 = CleanABData2 %>% select(
  Province,
  ABdefinitionSimple,
  Trainingsource,
  ABuse,
  ABfeed,
  ABUadvices,
  SecondABUadvices,
  ABsource,
  Drugcost,
  ABdosage,
  ABtrtlength,
  Measuresnotcured,
  Administerallchicken,
  Withdrawaltime,
  Leftover,
  Recordinformation.treatment
)

AbMCA = cbind(MCAABData3, ClusterTypo)

# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(AbMCA, plain.ascii = F, trim.strings = T) %>%  view


##### Perform MCA with Factoshinny

MCAtest = AbMCA
test <- MCAtest[, c(1:17)]  

#Uncomment and execute to launch a shiny app on your browser :
#MCAshiny(test)

### Perform MCA with FactoMineR with Province and ClusterTypo as Supplementary
res.mca <- MCA(AbMCA, quali.sup = c(1, 17), ncp = 8)
res.mca$eig

summary(res.mca)

res.hcpc2 <- HCPC(res.mca, consol = T, method = "ward") ### HCA
head(res.hcpc2$data.clust, 10)
mca1 <- res.hcpc2$data.clust ###selection cluster
summary(mca1$clust)
ClusterAB = mca1$clust  ## creation of the new variable: ClusterAB

AbMCA3 = cbind(AbMCA, ClusterAB)  ####Add th variable to a new dataset
# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(AbMCA3, plain.ascii = F, trim.strings = T) %>%  view

fviz_mca_biplot(
  res.mca,
  habillage = as.factor(AbMCA3$ClusterAB),
  col.var = "black",
  geom.ind = "point",
  axes = c(1, 2),
  repel = T,
  addEllipses = T,
  select.var = list(cos2 = 15),
  map = "symmetric",
  pointsize = 3,
  col.quali.sup = "red"
) #Create biplot showing the supplementary variables

fviz_cluster(res.hcpc2, ellipse.type = "convex") #### create Clusterplot

### Perform MCA with FactoShinny with farming practices as Supplementary
AbMCA2 = cbind(MCAABData3, SecMCA2TypoData)
# Uncomment and execute to get a tabular summary of the dataset (take a few minutes)
#dfSummary(AbMCA2, plain.ascii = F, trim.strings = T) %>%  view

MCAtest = AbMCA2
test <-
  MCAtest[, c(1:59)]   ## select the 15 AB variables as active and the remaining as supplementary

# Uncomment and execute to launch a shiny app  on your browser 
#MCAshiny(test)
