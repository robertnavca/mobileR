#' mobile.de_scraping
#'
#' @param url Enter an url copied from mobile.de web
#'
#' @return A data frame with all items avalaible in mobile.de
#' @export
#'
#' @examples mobile.de_scraping(url)
mobile.de_scraping <-
  function(url, lang = "en") {
    if (lang == "en") {
    webpage <- xml2::read_html(url)
    css_number_of_items <- ".rbt-result-list-headline"
    number_of_items <- rvest::html_node(webpage, css_number_of_items)
    text_number_of_items <- rvest::html_text(number_of_items)
    list_number_of_items <- strsplit(text_number_of_items, "[ ]")
    if (is.na(as.numeric(gsub("\\.", "", list_number_of_items[[1]][1]))) == FALSE) {
      number <- as.numeric(gsub("\\.", "", list_number_of_items[[1]][1]))
    } else {
      number <- as.numeric(gsub("\\,", "", list_number_of_items[[1]][1]))
    }
    number_of_pages <- ceiling(number/20)
    number_of_pages[number_of_pages > 50] <- 50
    dataframe <- data.frame()

    web_no_page <- strsplit(url, "pageNumber=")
    length_web_no_page <- as.numeric(length(web_no_page[[1]]))

    for (i in 1:number_of_pages) {
      if ((length_web_no_page > 1) == TRUE) {
        string2 <- substring(web_no_page[[1]][2], first = 2)
        url2 <- paste(web_no_page[[1]][1],"pageNumber=",i,string2, sep = "")
      } else {
        url2 <- paste(url,"&pageNumber=",i, sep = "")
      }

      webpage2 <- xml2::read_html(url2)
      css_model <- ".u-text-break-word"
      css_km <- ".rbt-regMilPow"
      css_vat <- ".u-block:nth-child(2)"
      css_price <- ".h3.u-block"
      css_class <- ".vehicle-data--ad-with-price-rating-label div:nth-child(2)"
      css_class2 <- ".g-col-12:nth-child(1) div:nth-child(1) div:nth-child(2)"
      css_valoration <- ".mde-price-rating__badge"
      # Extract nodes ---------------------------------------------
      model <- rvest::html_nodes(webpage2, css_model)
      km <- rvest::html_nodes(webpage2, css_km)
      vat <- rvest::html_nodes(webpage2, css_vat)
      price <- rvest::html_nodes(webpage2, css_price)
      class <- rvest::html_nodes(webpage2, css_class)
      class2 <- rvest::html_nodes(webpage2, css_class2)
      valoration <- rvest::html_nodes(webpage2, css_valoration)
      # Extract texts ---------------------------------------------
      text_model <- rvest::html_text(model)
      text_km <- rvest::html_text(km)
      text_vat <- rvest::html_text(vat)
      text_price <- rvest::html_text(price)
      text_class <- rvest::html_text(class)
      text_class2 <- rvest::html_text(class2)
      text_valoration <- rvest::html_text(valoration)
      # Model -----------------------------------------------------
      df_model <- as.data.frame(text_model)
      # Km & Year--------------------------------------------------
      df_km <- as.data.frame(text_km)
      df_km[df_km == ""] <- NA
      list_km <- strsplit(df_km$text_km, "[, ]")
      length_km <- as.numeric(length(list_km))
      Year <- data.frame()
      Km <- data.frame()
      Cv <- data.frame()
      for (i in 1:length_km) {
        vector <- as.vector(list_km[[i]])
        Cv[i,1] <- tail(vector, n = 1)
        if ("Neuwagen" %in% vector) {
          Year[i,1] <- "New/New"
          Km[i,1] <- vector[3]
        } else if ("Tageszulassung" %in% vector) {
          Year[i,1] <- "Pre-Registrstion/Pre-Registration"
        } else {
          Year[i,1] <- vector[2]
          Km[i,1] <- vector[4]
        }
      }
      list_year <- strsplit(Year[,1], "[/]")
      Year <- t(as.data.frame(list_year))
      Month <- as.data.frame(Year[,1])
      Year <- as.data.frame(Year[,2])

      list_km2 <- strsplit(Km[,1], "\\s")
      Km <- as.data.frame(as.data.frame(t(as.data.frame(list_km2)))[,1])

      # Cv --------------------------------------------------------
      list_cv <- strsplit(Cv[,1], "\\s+", )
      df_list_cv <- as.data.frame(list_cv)
      Kw <- as.data.frame(as.data.frame(t(as.data.frame(df_list_cv)))[,1])
      Hp <- as.data.frame(as.data.frame(t(as.data.frame(df_list_cv)))[,3])
      list_hp <- strsplit(Hp[,1], "[(]", )
      Hp <- as.data.frame(as.data.frame(t(as.data.frame(list_hp)))[,2])

      # Class -----------------------------------------------------
      df_class <- as.data.frame(text_class2)
      df_class <- as.data.frame(df_class[-1,])
      df_class <- as.data.frame(df_class[df_class[,1] != "", 1])
      list_class <- strsplit(df_class[,1], "[, ]")
      length_class <- as.numeric(length(list_class))
      Class <- data.frame()
      for (i in 1:length_class) {
        vector <- as.vector(list_class[[i]])
        # Fuel
        if ("Diesel" %in% vector)
          Class[i, 1] <- "Diesel"
        else if ("Benzin" %in% vector)
          Class[i, 1] <- "Petrol"
        else if ("Elektro" %in% vector)
          Class[i, 1] <- "Electric"
        else if ("(Benzin/Elektro)" %in% vector)
          Class[i, 1] <- "Hibrid (Petrol/Electric)"
        else if ("Erdgas" %in% vector)
          Class[i, 1] <- "Natural Gas"
        else if ("(LPG)" %in% vector)
          Class[i, 1] <- "LPG"
        else if ("(Diesel" %in% vector)
          Class[i, 1] <- "Hibrid (Diesel/Electric"
        else if ("Andere" %in% vector)
          Class[i, 1] <- "Other"
        else
          Class[i, 1] <- NA
        #Transmission
        if ("Automatik" %in% vector)
          Class[i, 2] <- "Automatic"
        else if ("Schaltgetriebe" %in% vector)
          Class[i, 2] <- "Manual"
        else
          Class[i, 2] <- NA
        # Accident Free
        if ("Unfallfrei" %in% vector)
          Class[i, 3] <- "Yes"
        else
          Class[i, 3] <- NA
        # Doors
        if ("4/5" %in% vector)
          Class[i, 4] <- "4/5"
        else if ("2/3" %in% vector)
          Class[i, 4] <- "2/3"
        else
          Class[i, 4] <- NA
        # Type
        if ("Sportwagen" %in% vector)
          Class[i, 5] <- "Sport Car / Coupe"
        else if ("SUV" %in% vector)
          Class[i, 5] <- "SUV / Off-Road / Pickup"
        else if ("Van" %in% vector)
          Class[i, 5] <- "Van / Minibus"
        else if ("Kombi" %in% vector)
          Class[i, 5] <- "Estate Car"
        else if ("Limousine" %in% vector)
          Class[i, 5] <- "Saloon"
        else if ("Cabrio" %in% vector)
          Class[i, 5] <- "Cabriolet"
        else if ("Kleinwagen" %in% vector)
          Class[i, 5] <- "Small Car"
        else if ("Andere" %in% vector[1:3])
          Class[i, 5] <- "Other"
        else
          Class[i, 5] <- NA
      }

      # Price -----------------------------------------------------
      Price <- as.data.frame(text_price)
      Price[Price == ""] <- NA
      list_price <- strsplit(Price[,1], "\\s+")
      Price <- as.data.frame(as.data.frame(t(as.data.frame(list_price)))[,1])
      Currency <- as.data.frame(as.data.frame(t(as.data.frame(list_price)))[,2])
      if (is.na(as.numeric(gsub("\\.", "", Price[,1]))) == FALSE) {
        Price <- as.data.frame(as.numeric(gsub("\\.", "", Price[,1])))
      } else {
        Price <- as.numeric(gsub("\\,", "", Price[,1]))}

      # Valoration ---------------------------------------------------------
      valoration <- as.data.frame(text_valoration)
      length_valoration <- as.numeric(length(text_valoration))
      if (length_valoration == 0) {
        df_valoration <- data.frame()}
      else if (length_class > length_valoration) {
        df_valoration <- data.frame()
        adds <- length_class - length_valoration
        df_valoration[1:adds, 1] <- NA
        no_adds <- adds + 1
        df_valoration[no_adds:length_class, 1] <- valoration[,1]
      } else {
        df_valoration <- valoration[,1]
      }
      if (length_valoration > 0) {
        df_valoration <- as.data.frame(df_valoration)
        df_valoration[,1][df_valoration[,1] == "Hoher Preis"] <- "*"
        df_valoration[,1][df_valoration[,1] == "Sehr guter Preis"] <- "* * * * *"
        df_valoration[,1][df_valoration[,1] == "Fairer Preis"] <- "* * *"
        df_valoration[,1][df_valoration[,1] == "Erhöhter Preis"] <- "* *"
        df_valoration[,1][df_valoration[,1] == "Guter Preis"] <- "* * * *"
        df_valoration[,1][df_valoration[,1] == "Ohne Bewertung"] <- "/"
        df_valoration[,1][is.null(df_valoration[,1]) == TRUE] <- NA

        df_general <- cbind(df_model,
                            Year,
                            Month,
                            Km,
                            Kw,
                            Hp,
                            Class,
                            Price,
                            df_valoration
        )
        colnames(df_general) <- c("Model", "Year", "Month", "Km", "Kw", "Hp", "Fuel", "Transmission", "Acc_Free", "Doors", "Type", "Price_€", "Price Valoration")
        df_general <- df_general[, c("Model", "Type", "Doors", "Year", "Month", "Kw", "Hp", "Km", "Fuel", "Transmission", "Acc_Free", "Price_€", "Price Valoration")]
        row.names(df_general) <- NULL
        dataframe <- rbind(dataframe, df_general)
      } else {
        df_general <- cbind(df_model,
                            Year,
                            Month,
                            Km,
                            Kw,
                            Hp,
                            Class,
                            Price
        )
        colnames(df_general) <- c("Title", "Year", "Month", "Km", "Kw", "Hp", "Fuel", "Transmission", "Acc_Free", "Doors", "Type", "Price_€")
        df_general <- df_general[, c("Title", "Type", "Doors", "Year", "Month", "Kw", "Hp", "Km", "Fuel", "Transmission", "Acc_Free", "Price_€")]
        row.names(df_general) <- NULL
        df_general$"Price Valoration" <- NA
        dataframe <- rbind(dataframe, df_general)
      }
    }
    #dataframe <- unique(dataframe)
    dataframe

  } else if (lang == "es") {

    # Spanish edition ----------------------------------------------
    webpage <- xml2::read_html(x)
    css_number_of_items <- ".h2"
    number_of_items <- rvest::html_node(webpage, css_number_of_items)
    text_number_of_items <- rvest::html_text(number_of_items)
    list_number_of_items <- strsplit(text_number_of_items, "[ ]")
    number <- list_number_of_items[[1]][1]
    number <- as.numeric(gsub("\\.", "", number))
    number_of_pages <- ceiling(number/50)
    number_of_pages[number_of_pages > 40] <- 40
    dataframe <- data.frame()

    web_no_page <- strsplit(x, ",pgn:")
    other_web_no_page <- strsplit(x, ",pgs")

    for (i in 1:number_of_pages) {
      if ((web_no_page[[1]][1] == other_web_no_page[[1]][1]) == FALSE) {
        url <- paste(web_no_page[[1]][1],",pgn:",i,",pgs",other_web_no_page[[1]][2], sep = "")
        } else {
        url <- paste(x,",pgn:",i,",pgs:50", sep = "") }

      webpage <- xml2::read_html(url)
      css_model <- ".u-text-nowrap"
      css_km <- ".g-col-s-6.g-col-m-8 .u-text-bold"
      css_cv <-  ".g-col-s-6.g-col-m-8 :nth-child(2)" # ".u-text-grey-60:nth-child(2)"
      css_price <- ".seller-currency.u-text-bold"
      css_class <- ".hidden-s .u-text-grey-60:nth-child(1)"
      # Extract nodes ---------------------------------------------
      model <- rvest::html_nodes(webpage, css_model)
      km <- rvest::html_nodes(webpage, css_km)
      cv <- rvest::html_nodes(webpage, css_cv)
      price <- rvest::html_nodes(webpage, css_price)
      class <- rvest::html_nodes(webpage, css_class)
      # Extract texts ---------------------------------------------
      text_model <- rvest::html_text(model)
      text_km <- rvest::html_text(km)
      text_cv <- rvest::html_text(cv)
      text_price <- rvest::html_text(price)
      text_class <- rvest::html_text(class)
      # Model -----------------------------------------------------
      df_model <- as.data.frame(text_model)
      Model <- df_model[-(1:25), ]
      # Km & Year--------------------------------------------------
      df_km <- as.data.frame(text_km)
      df_km[df_km == ""] <- "Nuevo/Nuevo, 0 km"
      list_km <- strsplit(df_km$text_km, ", ")
      Date <- data.frame()
      number_km <- as.numeric(length(list_km))
      for (i in 1:number_km) {
        lenght_line <- as.numeric(length(list_km[[i]]))
        if ((lenght_line < 2) == TRUE)  {
          Date[i, 1] <- "Nuevo/Nuevo"
          Date[i, 2] <- list_km[[i]][1]
        } else {
          Date[i, 1] <- list_km[[i]][1]
          Date[i, 2] <- list_km[[i]][2]
        }
      }
      list_year <- strsplit(Date[,1], "[/]")
      Year <- as.data.frame(as.data.frame(t(as.data.frame(list_year)))[,2])
      Month <- as.data.frame(as.data.frame(t(as.data.frame(list_year)))[,1])
      Km <- Date[,2]
      list_km <- strsplit(Km, "\\s+k")
      Km <- as.data.frame(as.data.frame(t(as.data.frame(list_km)))[,1])

      # Cv --------------------------------------------------------
      df_cv <- as.data.frame(text_cv)
      Cv <- df_cv[c(TRUE, FALSE),]
      Cv[Cv == ""] <- NA
      list_cv <- strsplit(Cv, "\\s+", )
      df_list_cv <- as.data.frame(list_cv)
      Cv <- as.data.frame(as.data.frame(t(as.data.frame(df_list_cv)))[,1])
      HP <- as.data.frame(as.data.frame(t(as.data.frame(df_list_cv)))[,3])
      ### Doors -----------------------------------------------------
      Doorstemp <- df_cv[c(FALSE, TRUE),]
      Doorstemp <- as.data.frame(Doorstemp)
      Doorstemp[Doorstemp == ""] <- NA
      list_doors <- strsplit(Doorstemp[,1], " ")
      Doors <- data.frame()
      for (i in 1:as.numeric(length(list_doors))) {
        vector <- list_doors[[i]]
        if ("2/3" %in% vector) {
          Doors[i, 1] <- "2/3"
        } else if ("4/5" %in% vector) {
          Doors[i, 1] <- "4/5"
        } else {
          Doors[i, 1] <- NA
        }
        if ("Blanco," %in% vector) {
          Doors[i, 2] <- "Blanco"
        } else if ("Negro," %in% vector) {
          Doors[i, 2] <- "Negro"
        } else if ("Rojo," %in% vector) {
          Doors[i, 2] <- "Rojo"
        } else if ("Azul," %in% vector) {
          Doors[i, 2] <- "Azul"
        } else if ("Plateado," %in% vector) {
          Doors[i, 2] <- "Plateado"
        } else if ("Gris," %in% vector) {
          Doors[i, 2] <- "Gris"
        } else if ("Beige," %in% vector) {
          Doors[i, 2] <- "Beige"
        } else if ("Marrón," %in% vector) {
          Doors[i, 2] <- "Marrón"
        } else if ("Dorado," %in% vector) {
          Doors[i, 2] <- "Dorado"
        } else if ("Naranja," %in% vector) {
          Doors[i, 2] <- "Naranja"
        } else if ("Verde," %in% vector) {
          Doors[i, 2] <- "Verde"
        } else if ("Morado," %in% vector) {
          Doors[i, 2] <- "Morado"
        } else {
          Doors[i, 2] <- NA
        }
      }

      # Class -----------------------------------------------------
      Classtemp <- as.data.frame(text_class)
      Classtemp[Classtemp == ""] <- NA
      list_class <- strsplit(Classtemp[,1], " ")
      Class <- data.frame()
      for (i in 1:as.numeric(length(list_class))) {
        vector <- list_class[[i]]
        if ("Diésel," %in% vector) {
          Class[i, 1] <- "Diésel"
        } else if ("Gasolina," %in% vector) {
          Class[i, 1] <- "Gasolina"
        } else if ("Eléctrico," %in% vector) {
          Class[i, 1] <- "Eléctrico"
        } else if ("(gasolina/eléctrico)," %in% vector) {
          Class[i, 1] <- "Híbrido (Gas/Elec)"
        } else if ("(diésel / eléctrico)," %in% vector) {
          Class[i, 1] <- "Híbrido (Dsl/Elec)"
        } else if ("natural," %in% vector) {
          Class[i, 1] <- "Gas Natural"
        } else if ("otro," %in% vector) {
          Class[i, 1] <- "Otro"
        } else {
          Class[i, 1] <- NA
        }
        # Cambio
        if ("automático" %in% vector) {
          Class[i, 2] <- "Automático"
        } else if ("manual" %in% vector) {
          Class[i, 2] <- "Manual"
        } else {
          Class[i, 2] <- NA
        }
        # Tipo
        if ("familiar," %in% vector) {
          Class[i, 3] <- "Familiar"
        } else if ("SUV" %in% vector) {
          Class[i, 3] <- "SUV / Todocamino"
        } else if ("pequeño," %in% vector) {
          Class[i, 3] <- "Pequeño"
        } else if ("Furgoneta" %in% vector) {
          Class[i, 3] <- "Furgoneta"
        } else if ("Sedán," %in% vector) {
          Class[i, 3] <- "Sedán"
        } else if ("Cabrio" %in% vector) {
          Class[i, 3] <- "Cabrio"
        } else if ("deportivo" %in% vector) {
          Class[i, 3] <- "Deportivo"
        } else if ("Otros" %in% vector) {
          Class[i, 3] <- "Otro"
        } else {
          Class[i, 3] <- NA
        }
      }
      # Price -----------------------------------------------------
      Price <- as.data.frame(text_price)
      Price[Price == ""] <- NA
      list_price <- strsplit(Price[,1], "\\s+")
      Price <- as.data.frame(as.data.frame(t(as.data.frame(list_price)))[,1])
      Price2 <- as.data.frame(as.data.frame(t(as.data.frame(list_price)))[,1])
      # Data Frame
      df <- data.frame(
        Model,
        Year,
        Month,
        Km,
        Cv,
        HP,
        Doors,
        Class,
        Price)
      colnames(df) <- c("Título", "Año", "Mes", "Km", "CV", "HP", "Puertas", "Color", "Combustible", "Cambio", "Tipo", "Precio")
      df$CV <- as.numeric(df$CV)
      df$Km <- as.numeric(gsub("\\.", "", df$Km))
      df$Precio <- as.numeric(gsub("\\.", "", df$Precio))
      df$HP <- substring(df$HP, 2)
      df$HP <- as.integer(df$HP)
      # df <- df[, colSums(is.na(df)) != nrow(df)]
      dataframe <- rbind(dataframe, df)
    }
    dataframe <- unique(dataframe)
    dataframe
  } else {
    return("Select a correct language between 'en' and 'es'.")
  }
  }
