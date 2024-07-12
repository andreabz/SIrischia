##### data for SQLite SIrischia.db ----
conn <- DBI::dbConnect(RSQLite::SQLite(),
                       "data/SIrischia.db",
                       extended_types = TRUE)

#### Tables in the Rilevability index ----
# PT
DBI::dbExecute(conn,
               "CREATE TABLE partecipazionept(
                  partecipazionept_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Due all''anno', 1);")
DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Uno all''anno', 2);")
DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Uno ogni due anni', 4);")
DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Uno ogni tre anni', 6);")
DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Uno ogni quattro anni', 8);")
DBI::dbExecute(conn,
               "INSERT INTO partecipazionept (descrizione, valore)
               VALUES ('Nessuno', 10);")

# Trueness
DBI::dbExecute(conn,
               "CREATE TABLE giustezza(
                  giustezza_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Prova qualitativa', 0);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('A ogni nuova sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Mensile o inferiore', 2);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 3);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Semestrale o inferiore', 5);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Annuale', 7);")
DBI::dbExecute(conn,
               "INSERT INTO giustezza (descrizione, valore)
               VALUES ('Pluriennale o nessuna verifica', 10);")

# Repeatability
DBI::dbExecute(conn,
               "CREATE TABLE ripetibilita(
                  ripetibilita_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO ripetibilita (descrizione, valore)
               VALUES ('A ogni sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO ripetibilita (descrizione, valore)
               VALUES ('Mensile o inferiore', 2);")
DBI::dbExecute(conn,
               "INSERT INTO ripetibilita (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 4);")
DBI::dbExecute(conn,
               "INSERT INTO ripetibilita (descrizione, valore)
               VALUES ('Semestrale o inferiore', 7);")
DBI::dbExecute(conn,
               "INSERT INTO ripetibilita (descrizione, valore)
               VALUES ('Annuale', 10);")

# QC charts
DBI::dbExecute(conn,
               "CREATE TABLE cartacontrollo(
                  cartacontrollo_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('Non rilevante', 0);")
DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('A ogni sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('Mensile o inferiore', 4);")
DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 6);")
DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('Semestrale o inferiore', 8);")
DBI::dbExecute(conn,
               "INSERT INTO cartacontrollo (descrizione, valore)
               VALUES ('Annuale o pluriennale', 10);")

# Selectivity
DBI::dbExecute(conn,
               "CREATE TABLE selettivita(
                  selettivita_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('Non rilevante', 0);")
DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('A ogni sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('Mensile o inferiore', 4);")
DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 6);")
DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('Semestrale o inferiore', 8);")
DBI::dbExecute(conn,
               "INSERT INTO selettivita (descrizione, valore)
               VALUES ('Annuale o pluriennale', 10);")

# Contamination
DBI::dbExecute(conn,
               "CREATE TABLE contaminazione(
                  contaminazione_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('Non rilevante', 0);")
DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('A ogni sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('Mensile o inferiore', 4);")
DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 6);")
DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('Semestrale o inferiore', 8);")
DBI::dbExecute(conn,
               "INSERT INTO contaminazione (descrizione, valore)
               VALUES ('Annuale o pluriennale', 10);")

# Calibration
DBI::dbExecute(conn,
               "CREATE TABLE taratura(
                  taratura_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('Non rilevante', 0);")
DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('A ogni sessione analitica', 1);")
DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('Mensile o inferiore', 4);")
DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('Trimestrale o inferiore', 6);")
DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('Semestrale o inferiore', 8);")
DBI::dbExecute(conn,
               "INSERT INTO taratura (descrizione, valore)
               VALUES ('Annuale o pluriennale', 10);")

#### Tables in the Probability index ----
# authorized personell
DBI::dbExecute(conn,
               "CREATE TABLE personale(
                  personale_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO personale (descrizione, valore)
               VALUES ('1', 10);")
DBI::dbExecute(conn,
               "INSERT INTO personale (descrizione, valore)
               VALUES ('2', 5);")
DBI::dbExecute(conn,
               "INSERT INTO personale (descrizione, valore)
               VALUES ('Più di 2', 1);")

# method type
DBI::dbExecute(conn,
               "CREATE TABLE metodotipo(
                  metodotipo_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO metodotipo (descrizione, valore)
               VALUES ('Normalizzato', 1);")
DBI::dbExecute(conn,
               "INSERT INTO metodotipo (descrizione, valore)
               VALUES ('Non normalizzato', 5);")
DBI::dbExecute(conn,
               "INSERT INTO metodotipo (descrizione, valore)
               VALUES ('Interno', 10);")

# maintenance
DBI::dbExecute(conn,
               "CREATE TABLE manutenzione(
                  manutenzione_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO manutenzione (descrizione, valore)
               VALUES ('Non rilevante', 10);")
DBI::dbExecute(conn,
               "INSERT INTO manutenzione (descrizione, valore)
               VALUES ('Nessuno', 1);")
DBI::dbExecute(conn,
               "INSERT INTO manutenzione (descrizione, valore)
               VALUES ('Tra uno e tre', 3);")
DBI::dbExecute(conn,
               "INSERT INTO manutenzione (descrizione, valore)
               VALUES ('Tra quattro e sei', 6);")
DBI::dbExecute(conn,
               "INSERT INTO manutenzione (descrizione, valore)
               VALUES ('Più di sei', 10);")

# pt results
DBI::dbExecute(conn,
               "CREATE TABLE risultatopt(
                  risultatopt_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Non rilevante', 0);")
DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Più di quattro anni', 1);")
DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Più di tre anni', 2);")
DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Più di due anni', 4);")
DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Più di un anno', 7);")
DBI::dbExecute(conn,
               "INSERT INTO risultatopt (descrizione, valore)
               VALUES ('Un anno o meno', 10);")

# samples per year
DBI::dbExecute(conn,
               "CREATE TABLE campionianno(
                  campionianno_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO campionianno (descrizione, valore)
               VALUES ('Meno di 10', 10);")
DBI::dbExecute(conn,
               "INSERT INTO campionianno (descrizione, valore)
               VALUES ('Tra 10 e 50', 5);")
DBI::dbExecute(conn,
               "INSERT INTO campionianno (descrizione, valore)
               VALUES ('Più di 50', 1);")

# accredited method
DBI::dbExecute(conn,
               "CREATE TABLE metodoaccreditato(
                  metodoaccreditato_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO metodoaccreditato (descrizione, valore)
               VALUES ('Sì', 1);")
DBI::dbExecute(conn,
               "INSERT INTO metodoaccreditato (descrizione, valore)
               VALUES ('No', 10);")

# result mistakes
DBI::dbExecute(conn,
               "CREATE TABLE erroririsultato(
                  erroririsultato_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO erroririsultato (descrizione, valore)
               VALUES ('Trasferimento dati automatizzato', 1);")
DBI::dbExecute(conn,
               "INSERT INTO erroririsultato (descrizione, valore)
               VALUES ('Nessuno', 1);")
DBI::dbExecute(conn,
               "INSERT INTO erroririsultato (descrizione, valore)
               VALUES ('Raramente', 6);")
DBI::dbExecute(conn,
               "INSERT INTO erroririsultato (descrizione, valore)
               VALUES ('Saltuariamente', 8);")
DBI::dbExecute(conn,
               "INSERT INTO erroririsultato (descrizione, valore)
               VALUES ('Spesso', 10);")

# method experience
DBI::dbExecute(conn,
               "CREATE TABLE esperienzametodo(
                  esperienzametodo_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO esperienzametodo (descrizione, valore)
               VALUES ('Più di cinque', 1);")
DBI::dbExecute(conn,
               "INSERT INTO esperienzametodo (descrizione, valore)
               VALUES ('Tra uno e cinque', 3);")
DBI::dbExecute(conn,
               "INSERT INTO esperienzametodo (descrizione, valore)
               VALUES ('Un anno o meno', 6);")
DBI::dbExecute(conn,
               "INSERT INTO esperienzametodo (descrizione, valore)
               VALUES ('Nuovo metodo', 10);")

#### Tables in the Magnitude index ----
# sanctioning trial
DBI::dbExecute(conn,
               "CREATE TABLE provasanzionatoria(
                  provasanzionatoria_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO provasanzionatoria (descrizione, valore)
               VALUES ('Sì', 10);")
DBI::dbExecute(conn,
               "INSERT INTO provasanzionatoria (descrizione, valore)
               VALUES ('No', 1);")

# reputational damage
DBI::dbExecute(conn,
               "CREATE TABLE dannoimmagine(
                  dannoimmagine_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO dannoimmagine (descrizione, valore)
               VALUES ('Alto', 10);")
DBI::dbExecute(conn,
               "INSERT INTO dannoimmagine (descrizione, valore)
               VALUES ('Medio', 5);")
DBI::dbExecute(conn,
               "INSERT INTO dannoimmagine (descrizione, valore)
               VALUES ('Basso', 1);")

# client reputational damage
DBI::dbExecute(conn,
               "CREATE TABLE perditafiducia(
                  perditafiducia_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO perditafiducia (descrizione, valore)
               VALUES ('Alto', 10);")
DBI::dbExecute(conn,
               "INSERT INTO perditafiducia (descrizione, valore)
               VALUES ('Medio', 5);")
DBI::dbExecute(conn,
               "INSERT INTO perditafiducia (descrizione, valore)
               VALUES ('Basso', 1);")

# request not satisfied
DBI::dbExecute(conn,
               "CREATE TABLE fattoriaggravanti(
                  fattoriaggravanti_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO fattoriaggravanti (descrizione, valore)
               VALUES ('Sì', 10);")
DBI::dbExecute(conn,
               "INSERT INTO fattoriaggravanti (descrizione, valore)
               VALUES ('Parzialmente', 5);")
DBI::dbExecute(conn,
               "INSERT INTO fattoriaggravanti (descrizione, valore)
               VALUES ('No', 1);")

# repeatable test
DBI::dbExecute(conn,
               "CREATE TABLE provaripetibile(
                  provaripetibile_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  descrizione TEXT NOT NULL,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO provaripetibile (descrizione, valore)
               VALUES ('Sì, sullo stesso materiale', 10);")
DBI::dbExecute(conn,
               "INSERT INTO provaripetibile (descrizione, valore)
               VALUES ('Sì, dopo nuovo campionamento', 5);")
DBI::dbExecute(conn,
               "INSERT INTO provaripetibile (descrizione, valore)
               VALUES ('No', 1);")

# labsites
DBI::dbExecute(conn,
               "CREATE TABLE sede(
                  sede_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore TEXT NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO sede (valore)
               VALUES ('Imperia');")
DBI::dbExecute(conn,
               "INSERT INTO sede (valore)
               VALUES ('Savona');")
DBI::dbExecute(conn,
               "INSERT INTO sede (valore)
               VALUES ('Genova');")
DBI::dbExecute(conn,
               "INSERT INTO sede (valore)
               VALUES ('La Spezia');")

# area
DBI::dbExecute(conn,
               "CREATE TABLE settore(
                  settore_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore TEXT NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('biologia centro levante');")
DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('biologia metropolitano');")
DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('biologia ponente');")
DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('chimica levante');")
DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('chimica metropolitano');")
DBI::dbExecute(conn,
               "INSERT INTO settore (valore)
               VALUES ('chimica organica');")

# unit
DBI::dbExecute(conn,
               "CREATE TABLE unitaoperativa(
                  unitaoperativa_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore TEXT NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO unitaoperativa (valore)
               VALUES ('accettazione e coordinamento servizi laboratoristici');")
DBI::dbExecute(conn,
               "INSERT INTO unitaoperativa (valore)
               VALUES ('biologia e tossicologia');")
DBI::dbExecute(conn,
               "INSERT INTO unitaoperativa (valore)
               VALUES ('chimica 1');")
DBI::dbExecute(conn,
               "INSERT INTO unitaoperativa (valore)
               VALUES ('chimica 2');")

# matrix
DBI::dbExecute(conn,
               "CREATE TABLE matrice(
                  matrice_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore TEXT NOT NULL
                );")

DBI::dbWriteTable(conn,
                   "matrice",
                   data.table::fread("data-raw/matrice.csv"),
                  append = TRUE)

# technique
DBI::dbExecute(conn,
               "CREATE TABLE tecnica(
                  tecnica_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore TEXT NOT NULL
                );")

DBI::dbWriteTable(conn,
                  "tecnica",
                  data.table::fread("data-raw/tecnica.csv"),
                  append = TRUE)

# method
DBI::dbExecute(conn,
               "CREATE TABLE metodo(
                  metodo_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  sede_id INTEGER REFERENCES sede(sede_id),
                  unitaoperativa_id INTEGER REFERENCES unitaoperativa(unitaoperativa_id),
                  settore_id INTEGER REFERENCES settore(settore_id),
                  tecnica_id INTEGER REFERENCES tecnica(tecnica_id),
                  metodo TEXT NOT NULL,
                  misurando TEXT NOT NULL,
                  riferimento TEXT NOT NULL,
                  tecnici_autorizzati TEXT NOT NULL,
                  apparecchiature_critiche TEXT NOT NULL
                );")

mytable <- data.table::fread("data-raw/metodo.csv")
mytable <- mytable[, lapply(.SD, gsub, pattern = "[\r\n]", replacement = " "), .SDcols = colnames(mytable)]
mytable <- mytable[, lapply(.SD, gsub, pattern = "[\n]", replacement = " "), .SDcols = colnames(mytable)]
mytable <- mytable[, lapply(.SD, gsub, pattern = "[\r]", replacement = " "), .SDcols = colnames(mytable)]
mytable <- mytable[, lapply(.SD, gsub, pattern = "  ", replacement = " "), .SDcols = colnames(mytable)]
mytable <- mytable[, lapply(.SD, gsub, pattern = ",([A-Za-z])", replacement = ", \\1"), .SDcols = colnames(mytable)]

DBI::dbWriteTable(conn,
                  "metodo",
                  mytable,
                  append = TRUE)

# metodo_matrice
DBI::dbExecute(conn,
               "CREATE TABLE metodo_matrice(
                  metodo_matrice_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  metodo_id INTEGER REFERENCES metodo(metodo_id),
                  matrice_id INTEGER REFERENCES matrice(matrice_id)
                );")

DBI::dbWriteTable(conn,
                  "metodo_matrice",
                  data.table::fread("data-raw/metodo_matrice.csv"),
                  append = TRUE)

# year
DBI::dbExecute(conn,
               "CREATE TABLE anno(
                  anno_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  valore INTEGER NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2024);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2025);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2026);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2027);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2028);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2029);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2030);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2031);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2032);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2033);")
DBI::dbExecute(conn,
               "INSERT INTO anno (valore)
               VALUES (2034);")

#### Result tables ----
# rilevability table
DBI::dbExecute(conn,
               "CREATE TABLE rilevabilita(
                  rilevabilita_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  metodo_id INTEGER REFERENCES metodo(metodo_id),
                  anno_id INTEGER REFERENCES anno(anno_id),
                  partecipazionept_id INTEGER REFERENCES partecipazionept(partecipazionept_id),
                  giustezza_id INTEGER REFERENCES giustezza(giustezza_id),
                  ripetibilita_id INTEGER REFERENCES ripetibilita(ripetibilita_id),
                  cartacontrollo_id INTEGER REFERENCES cartacontrollo(cartacontrollo_id),
                  selettivita_id INTEGER REFERENCES selettivita(selettivita_id),
                  contaminazione_id INTEGER REFERENCES contaminazione(contaminazione_id),
                  taratura_id INTEGER REFERENCES taratura(taratura_id)
                );")

# probability table
DBI::dbExecute(conn,
               "CREATE TABLE probabilita(
                  probabilita_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  metodo_id INTEGER REFERENCES metodo(metodo_id),
                  anno_id INTEGER REFERENCES anno(anno_id),
                  personale_id INTEGER REFERENCES personale(personale_id),
                  metodotipo_id INTEGER REFERENCES metodotipo(metodotipo_id),
                  manutenzione_id INTEGER REFERENCES manutenzione(manutenzione_id),
                  risultatopt_id INTEGER REFERENCES risultatopt(risultatopt_id),
                  campionianno_id INTEGER REFERENCES campionianno(campionianno_id),
                  metodoaccreditato_id INTEGER REFERENCES metodoaccreditato(metodoaccreditato_id),
                  erroririsultato_id INTEGER REFERENCES erroririsultato(erroririsultato_id),
                  esperienzametodo_id INTEGER REFERENCES esperienzametodo(esperienzametodo_id)
                );")

# magnitude table
DBI::dbExecute(conn,
               "CREATE TABLE gravita(
                  gravita_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  metodo_id INTEGER REFERENCES metodo(metodo_id),
                  anno_id INTEGER REFERENCES anno(anno_id),
                  provasanzionatoria_id INTEGER REFERENCES provasanzionatoria(provasanzionatoria_id),
                  dannoimmagine_id INTEGER REFERENCES dannoimmagine(dannoimmagine_id),
                  perditafiducia_id INTEGER REFERENCES perditafiducia(perditafiducia_id),
                  fattoriaggravanti_id INTEGER REFERENCES fattoriaggravanti(fattoriaggravanti_id),
                  provaripetibile_id INTEGER REFERENCES provaripetibile(provaripetibile_id)
                );")

# risk table
DBI::dbExecute(conn,
               "CREATE TABLE rischio(
                  rischio_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  rilevabilita_id INTEGER REFERENCES rilevabilita(rilevabilita_id),
                  probabilita_id INTEGER REFERENCES probabilita(probabilita_id),
                  gravita_id INTEGER REFERENCES gravita(gravita_id)
                );")
