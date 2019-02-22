
options(stringsAsFactors = FALSE);

# library(sqldf);

propojeniRok <- function(rok) {
  
  df_kandidati <- get(paste0('kvrk_', rok));

  if (rok >= 2006) {
    df_obce <- get(paste0('kvrzcoco_', rok));
    df_strany <- get(paste0('kvros_', rok));
  } else {
    df_obce <- kvrzcoco_2006;
    df_strany <- kvros_2006;
  }
  
  # u staturárních měst je pod jedním KODZAST víc NAZEVOBCE, zajímá mě ale jen NAZEVZAST,   
  # takže vezmu jen první NAZEVOBCE, ostatní vyhodím
  df <- merge(df_kandidati, df_obce, by = c('KODZASTUP','COBVODU'), all.x = T);
  df <- df[!duplicated(data.frame(df$KODZASTUP, df$OSTRANA, df$PORCISLO, df$JMENO, 
                                  df$PRIJMENI)),];
  
  df <- merge(df, df_strany, by = c('KODZASTUP','COBVODU','OSTRANA'), all.x = T);

  # texty raději uppercase
  df$NAZEVNUTSREG <- toupper(df$NAZEVNUTSREG);
  df$NAZEVNUTSKRAJ <- toupper(df$NAZEVNUTSKRAJ);
  df$NAZEVNUTSOKRES <- toupper(df$NAZEVNUTSOKRES);
  df$NAZEVZAST.x <- toupper(df$NAZEVZAST.x);
  df$NAZEVCELK <- toupper(df$NAZEVCELK);
  df$ZKRATKAO30 <- toupper(df$ZKRATKAO30);
  df$ZKRATKAO8 <- toupper(df$ZKRATKAO8);
  df$JMENO <- toupper(df$JMENO);
  df$PRIJMENI <- toupper(df$PRIJMENI);
  df$TITULPRED <- toupper(df$TITULPRED);
  df$TITULZA <- toupper(df$TITULZA);
  df$POVOLANI <- toupper(df$POVOLANI);
  df$BYDLISTEN <- toupper(df$BYDLISTEN);
  df$NAZEVOBCE <- toupper(df$NAZEVOBCE);
  
  # u názvů stran uvozovky pryč
  df$NAZEVCELK <- gsub('"', '', df$NAZEVCELK, fixed = T);
  
  # seřazení sloupců (logika místo|strana|kandidát|ostatní proměnné)
  if (rok == 2018) {

    df <- data.frame(df$NAZEVNUTSREG, df$NAZEVNUTSKRAJ, df$NAZEVNUTSOKRES, df$NAZEVZAST.x, 
                     df$COBVODU, df$NAZEVCELK, df$ZKRATKAO30, df$ZKRATKAO8, df$PORCISLO, 
                     df$JMENO, df$PRIJMENI, df$ZENA, df$TITULPRED, df$TITULZA, df$VS, 
                     df$VEK, df$POVOLANI, df$BYDLISTEN, df$MANDAT, df$VSTRANA, df$OSTRANA, 
                     df$PSTRANA, df$NSTRANA, df$POCSTR_SLO, df$SLOZENI, df$KODZASTUP,
                     df$TYPZASTUP.x, df$DRUHZASTUP, df$OBVODY, df$MANDATY, df$PORADIMAND, 
                     df$PORADINAHR, df$MAND_STR, df$POCHLASU, df$POCHL_PRES, 
                     df$POCPROCVSE, df$HLASY_STR, df$PROCHLSTR, df$POR_STR_HL.x,
                     df$OBEC, df$NAZEVOBCE, df$ORP, df$CPOU, df$REGURAD, df$POCOBYV, 
                     df$PLATNOST, df$MINOKRSEK1, df$MAXOKRSEK1, df$MINOKRSEK2, 
                     df$MAXOKRSEK2, df$MINOKRSEK3, df$MAXOKRSEK3, df$MINOKRSEK4, 
                     df$MAXOKRSEK4, df$MINOKRSEK5, df$MAXOKRSEK5, df$MINOKRSEK6, 
                     df$MAXOKRSEK6, df$MINOKRSEK7, df$MAXOKRSEK7, df$MINOKRSEK8, 
                     df$MAXOKRSEK8, df$MINOKRSEK9, df$MAXOKRSEK9, df$MINOKRSE10, 
                     df$MAXOKRSE10, df$TYPDUVODU, df$POCET_VS, df$STAV_OBCE);
  } else {

    df <- data.frame(df$NAZEVNUTSREG, df$NAZEVNUTSKRAJ, df$NAZEVNUTSOKRES, df$NAZEVZAST.x, 
                     df$COBVODU, df$NAZEVCELK, df$ZKRATKAO30, df$ZKRATKAO8, df$PORCISLO, 
                     df$JMENO, df$PRIJMENI, df$ZENA, df$TITULPRED, df$TITULZA, df$VS, 
                     df$VEK, df$POVOLANI, df$BYDLISTEN, df$MANDAT, df$VSTRANA, df$OSTRANA, 
                     df$PSTRANA, df$NSTRANA, df$POCSTR_SLO, df$SLOZENI, df$KODZASTUP,
                     df$TYPZASTUP.x, df$DRUHZASTUP, df$OBVODY, df$MANDATY, df$PORADIMAND, 
                     df$PORADINAHR, df$MAND_STR, df$POCHLASU, df$POCHL_PRES, 
                     df$POCPROCVSE, df$HLASY_STR, df$PROCHLSTR, df$POR_STR_HL.x,
                     df$OBEC, df$NAZEVOBCE, df$COBROP, df$CPOU, df$REGURAD, df$POCOBYV, 
                     df$PLATNOST, df$MINOKRSEK1, df$MAXOKRSEK1, df$MINOKRSEK2, 
                     df$MAXOKRSEK2, df$MINOKRSEK3, df$MAXOKRSEK3, df$MINOKRSEK4, 
                     df$MAXOKRSEK4, df$MINOKRSEK5, df$MAXOKRSEK5, df$MINOKRSEK6, 
                     df$MAXOKRSEK6, df$MINOKRSEK7, df$MAXOKRSEK7, df$MINOKRSEK8, 
                     df$MAXOKRSEK8, df$MINOKRSEK9, df$MAXOKRSEK9, df$MINOKRSE10, 
                     df$MAXOKRSE10, df$TYPDUVODU, df$POCET_VS, df$STAV_OBCE);
  }

  colnames(df) <- c('REGION', 'KRAJ', 'OKRES', 'NAZEVZAST', 'COBVODU', 'NAZEVCELK',
                    'ZKRATKA30', 'ZKRATKA8', 'PORCISLO', 'JMENO', 'PRIJMENI', 'ZENA',  
                    'TITULPRED', 'TITULZA', 'VS', 'VEK', 'POVOLANI', 'BYDLISTEN',
                    'MANDAT', 'VSTRANA', 'OSTRANA', 'PSTRANA', 'NSTRANA',  
                    'POCSTR_SLO', 'SLOZENI', 'KODZASTUP', 'TYPZASTUP', 'DRUHZASTUP',
                    'OBVODY', 'MANDATY', 'PORADIMAND', 'PORADINAHR', 'MAND_STR',
                    'POCHLASU', 'POCHL_PRES', 'POCPROCVSE', 'HLASY_STR', 'PROCHLSTR',
                    'POR_STR_HL', 'OBEC', 'NAZEVOBCE', 'ORP', 'CPOU', 'REGURAD', 'POCOBYV',
                    'PLATNOST', 'MINOKRSEK1', 'MAXOKRSEK1', 'MINOKRSEK2', 'MAXOKRSEK2',
                    'MINOKRSEK3', 'MAXOKRSEK3', 'MINOKRSEK4', 'MAXOKRSEK4', 'MINOKRSEK5',
                    'MAXOKRSEK5', 'MINOKRSEK6', 'MAXOKRSEK6', 'MINOKRSEK7', 'MAXOKRSEK7',
                    'MINOKRSEK8', 'MAXOKRSEK8', 'MINOKRSEK9', 'MAXOKRSEK9', 'MINOKRSE10',
                    'MAXOKRSE10', 'TYPDUVODU', 'POCET_VS', 'STAV_OBCE');
  
  return(df);

}


# propojení dvojích voleb; pozdější volby jsou první parametr
propojeniCasovaRada <- function(df1, df2) {

  # varianta s range joinem na věk 3 až 5 let; nefunguje, málo paměti 
  # sqldf("select * from x x1 inner join y y1
  #       on (x1.VEK - y1.VEK <= 5 and x1.VEK - y1.VEK >= 3) ")

  rok1 <- substr(df1, 4, 6);
  rok2 <- substr(df2, 4, 6);
  
  x <- get(df1);
  y <- get(df2);
  
  if(rok2 %in% c('14', '10', '06', '02')) {
    rozdilVeku <- 2018 - as.numeric(paste0('20', rok2));
  } else {
    rozdilVeku <- 2018 - as.numeric(paste0('19', rok2));
  }
  
  y$VEK <- y$VEK + rozdilVeku;
  
  if(rok1 == '') {
    colnames(x)[3] <- ('VEK');
  }

  df <- merge(x, y, by=c('JMENO', 'PRIJMENI', 'VEK', 'KODZASTUP'), all = T);
  
  sloupce <- c('REGION', 'KRAJ', 'OKRES', 'NAZEVZAST', 'COBVODU', 'NAZEVCELK',
               'ZKRATKA30', 'ZKRATKA8', 'PORCISLO', 'ZENA',  
               'TITULPRED', 'TITULZA', 'VS', 'POVOLANI', 'BYDLISTEN',
               'MANDAT', 'VSTRANA', 'OSTRANA', 'PSTRANA', 'NSTRANA',  
               'POCSTR_SLO', 'SLOZENI', 'TYPZASTUP', 'DRUHZASTUP',
               'OBVODY', 'MANDATY', 'PORADIMAND', 'PORADINAHR', 'MAND_STR',
               'POCHLASU', 'POCHL_PRES', 'POCPROCVSE', 'HLASY_STR', 'PROCHLSTR',
               'POR_STR_HL', 'OBEC', 'NAZEVOBCE', 'ORP', 'CPOU', 'REGURAD', 'POCOBYV',
               'PLATNOST', 'MINOKRSEK1', 'MAXOKRSEK1', 'MINOKRSEK2', 'MAXOKRSEK2',
               'MINOKRSEK3', 'MAXOKRSEK3', 'MINOKRSEK4', 'MAXOKRSEK4', 'MINOKRSEK5',
               'MAXOKRSEK5', 'MINOKRSEK6', 'MAXOKRSEK6', 'MINOKRSEK7', 'MAXOKRSEK7',
               'MINOKRSEK8', 'MAXOKRSEK8', 'MINOKRSEK9', 'MAXOKRSEK9', 'MINOKRSE10',
               'MAXOKRSE10', 'TYPDUVODU', 'POCET_VS', 'STAV_OBCE');
  
  sloupceNew <- paste0(sloupce, rok1);
  sloupceOld <- paste0(sloupce, rok2);
  
  colnames(df)[3] <- 'VEK18';
  
  if(rok1 != '') {
    colnames(df)[5:69] <- sloupceNew;
  }
  
  colnames(df)[(ncol(df)-64):ncol(df)] <- sloupceOld;

  df[paste0('VEK', rok2)] <- df[3] - rozdilVeku;
  
  return(df);

}



df_18 <- propojeniRok(2018);
df_14 <- propojeniRok(2014);
df_10 <- propojeniRok(2010);
df_06 <- propojeniRok(2006);

df <- propojeniCasovaRada('df_18', 'df_14');
df <- propojeniCasovaRada('df', 'df_10');
df <- propojeniCasovaRada('df', 'df_06');
