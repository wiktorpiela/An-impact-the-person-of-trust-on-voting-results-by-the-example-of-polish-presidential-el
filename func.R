library("tidyverse")
library("readxl")
library("patchwork")

prepare_results <- function(path){
  
  df <- read_excel(paste0("data/",path)) %>% 
    filter(`Typ obwodu` == "sta�y") %>% 
    select(`Kod TERYT`,`Numer obwodu`,`Typ obszaru`,`Typ obwodu`,Siedziba,Gmina,Powiat,Wojew�dztwo,Frekwencja,`% g�os�w niewa�nych`,
           `Andrzej Sebastian DUDA`,`Rafa� Kazimierz TRZASKOWSKI`) %>% 
    mutate(klucz = paste0(`Kod TERYT`, `Numer obwodu`)) %>% 
    filter(!is.na(`Andrzej Sebastian DUDA`))
  
  return(df)
}

prepare_trustee_data <- function(path){
  
  df <- read_excel(paste0("data/",path)) %>% 
    rename(maz = "Adnotacja o obecno�ci m��w zaufania w obwodzie") %>% 
    mutate(maz = as.factor(maz),
           maz = fct_lump(maz,1),
           klucz = paste0(`Kod TERYT`, `Numer obwodu`)) %>% 
    select(klucz, maz)
  
  return(df)
}

prepare_population_data <- function(path){
  
  df <- read_excel(paste0("data/",path)) %>% 
    mutate(klucz = paste0(`Kod TERYT`,`Numer obwodu`)) %>% 
    select(klucz, `Liczba wyborc�w uprawnionych do g�osowania`) %>% 
    rename(populacja = "Liczba wyborc�w uprawnionych do g�osowania")
  
  return(df)
}

convert_invalid_votes_1 <- function(path){
  
  df <- read_excel(paste0("data/",path)) %>% 
    select(`Kod TERYT`,`Numer obwodu`,`% g�os�w niewa�nych`,
           `W tym z powodu postawienia znaku �X� obok nazwiska dw�ch lub wi�kszej liczby kandydat�w`,
           `W tym z powodu niepostawienia znaku �X� obok nazwiska �adnego kandydata`) %>% 
    rename(x2 = "W tym z powodu postawienia znaku �X� obok nazwiska dw�ch lub wi�kszej liczby kandydat�w",
           x0 = "W tym z powodu niepostawienia znaku �X� obok nazwiska �adnego kandydata") %>% 
    mutate(`% g�os�w niewa�nych` = replace_na(`% g�os�w niewa�nych`,0),
           klucz = paste0(`Kod TERYT`,`Numer obwodu`),
           x2 = ifelse(`% g�os�w niewa�nych` == 0, 0,x2),
           x0 = ifelse(`% g�os�w niewa�nych` == 0, 0,x0),
           #zamiana na procenty
           x2 = x2/100,
           x0 = x0/100,
           #zamiana z procent w tym na procent ogolem
           x2 = x2*`% g�os�w niewa�nych`,
           x0 = x0*`% g�os�w niewa�nych`
    ) %>% 
    rename(x2_1 = "x2",
           x0_1 = "x0") %>% 
    select(klucz, x2_1,x0_1)
  
  return(df)
}

convert_invalid_votes_2 <- function(path){
  
  df <-  read_excel(paste0("data/",path)) %>% 
    select(`Kod TERYT`,`Numer obwodu`,`% g�os�w niewa�nych`,
           `W tym z powodu postawienia znaku �X� obok nazwiska dw�ch lub wi�kszej liczby kandydat�w`,
           `W tym z powodu niepostawienia znaku �X� obok nazwiska �adnego kandydata`) %>% 
    rename(x2 = "W tym z powodu postawienia znaku �X� obok nazwiska dw�ch lub wi�kszej liczby kandydat�w",
           x0 = "W tym z powodu niepostawienia znaku �X� obok nazwiska �adnego kandydata") %>% 
    mutate(`% g�os�w niewa�nych` = replace_na(`% g�os�w niewa�nych`,0),
           klucz = paste0(`Kod TERYT`,`Numer obwodu`),
           x2 = ifelse(`% g�os�w niewa�nych` == 0, 0,x2),
           x0 = ifelse(`% g�os�w niewa�nych` == 0, 0,x0),
           #zamiana na procenty
           x2 = x2/100,
           x0 = x0/100,
           #zamiana z procent w tym na procent ogolem
           x2 = x2*`% g�os�w niewa�nych`,
           x0 = x0*`% g�os�w niewa�nych`
    ) %>% 
    rename(x2_2 = "x2",
           x0_2 = "x0") %>% 
    select(klucz, x2_2, x0_2)
  
  return(df)
}


trustee_comparison1 <- function(df){
  
  df <- df %>% 
    pivot_longer(cols=c(maz_1,maz_2),
                 names_to="tura",
                 values_to = "obecnosc") %>% 
    group_by(tura) %>% 
    count(obecnosc) %>% 
    mutate(prop = n/sum(n))
  
  return(df) 
}

trustee_comparison2 <- function(df){
  
  df <- df %>% 
    pivot_longer(cols=c(maz_1,maz_2),
                 names_to="tura",
                 values_to = "obecnosc") %>% 
    group_by(`Typ obszaru`,tura) %>% 
    count(obecnosc) %>% 
    mutate(prop = n/sum(n)) %>% 
    ungroup()
  
  return(df)
  
}

trustee_comparison3 <- function(df){
  
  df <- df %>% 
    pivot_longer(cols=c(maz_1,maz_2),
                 names_to="tura",
                 values_to = "obecnosc") %>% 
    group_by(Wojew�dztwo,`Typ obszaru`,tura) %>% 
    count(obecnosc) %>% 
    mutate(prop = n/sum(n)) %>% 
    ungroup()
  
  return(df)
  
}

get_runoff_results <- function(){
  
  df2015 <- read_excel("data/wyniki_tura2_2015.xls")
  
  df2015 <- df2015[,c(ncol(df2015)-1,ncol(df2015))] %>% 
    rename(`Andrzej Sebastian DUDA` = "Andrzej Sebastian Duda",
           `Bronis�aw Maria KOMOROWSKI` = "Bronis�aw Maria Komorowski") %>% 
    pivot_longer(cols = c(1,2),
                 names_to = "kandydat",
                 values_to = "wynik") %>% 
    group_by(kandydat) %>%
    summarise(sum(wynik)) %>%
    mutate(prop = `sum(wynik)`/sum(`sum(wynik)`),
           wybory = "2015") 
  
  df2020 <- read_excel("data/wyniki_tura2_2020.xlsx")
  
  df2020 <- df2020[,c(ncol(df2020)-1,ncol(df2020))] %>% 
    pivot_longer(cols = 1:2,
                 names_to = "kandydat",
                 values_to = "wynik") %>% 
    filter(wynik!="-") %>% 
    group_by(kandydat) %>% 
    summarise(sum(wynik)) %>% 
    ungroup() %>% 
    mutate(prop = `sum(wynik)`/sum(`sum(wynik)`),
           wybory = "2020")
  
  return(bind_rows(df2015, df2020))

}