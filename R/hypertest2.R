### hypergeometric enrichment testing
hypertest2 <- function(besondere,gezogene,hintergrund, more=T, unique=T){   #besondere  zu testen auf anreicherung(more=T) oder abreicherung (more=F) in gezogene bei gegebenem Hintergrund hintergrund. besondere,gezogene und hintergrund koennen die laengen der listen sein oder die listen selber
  #http://r.789695.n4.nabble.com/hypergeometric-vs-fisher-test-tt2324223.html
  #in diesem www-Beispiel sind die weissen Kugeln a+b und entsprechen allen Elemente der eingabe "besondere", z.b. allen eQTL SNPs
  #alle gezogenen Kugeln sind a+c und entsprechen der Eingabe "gezogene" und sind z.b. alle GWAS SNPs, egal ob eQTL oder nich eQTL
  #alle Elemente der Urne sind a+b+c+d und entsprechen dem Hintergrund "hintergrund", e.g. allen dbSNP eintraegen mit dem gleichen unspezifischen Filtern wie die Liste (e.g. alle dbSNP SNPs, die auf eQTL getestet wurden)
    # if (all(is.character(besondere), is.character(gezogene),
            # is.character(hintergrund)) == F)
      # warning("input nicht vom typ chr ",immediate. = F)

    if (unique == T) {
      besondere <- unique(besondere)
      gezogene <- unique(gezogene)
      hintergrund <- unique(hintergrund)
    }

    besondere_inbg = besondere[besondere %in% hintergrund]
    message("Schraenke 'besondere' auf 'hintergrund' ein: Using ", length(besondere_inbg) , " instead of ", length(besondere))
    besondere = besondere_inbg

    gezogene_inbg = gezogene[gezogene %in% hintergrund]
    message("Schraenke 'gezogene' auf 'hintergrund' ein: Using ", length(gezogene_inbg) , " instead of ", length(gezogene))
    gezogene = gezogene_inbg


    if (all(besondere %in% hintergrund) == F)
      stop("nicht alle 'besondere' (alle weissen Kugeln) in 'hintergrund' (der Hintergrund)")
    if (all(gezogene %in% hintergrund) == F)
      stop("nicht alle 'gezogene' (alle gezogenen Kugeln) in 'hintergrund' (der Hintergrund)")
    if (any(is.na(besondere), is.na(besondere), is.na(besondere)) ==
        T)
      stop("NA in den daten versteckt!")

    aa <- sum(besondere %in% gezogene)
    bb <- length(besondere) - aa
    cc <- length(gezogene) - aa
    dd <- length(hintergrund) - cc - bb - aa
    pval = phyper(aa - 1, aa + bb, cc + dd, aa + cc, lower.tail = !more)
    in_gezogen <- round((aa/(aa + cc)) * 100, 3)
    in_bk <- round(((aa + bb)/(aa + bb + cc + dd)) * 100, 3)
    enr <- round(in_gezogen/in_bk, 3)
    mymatrix = matrix(c(aa, bb, cc, dd), nrow = 2)
    or = fisher.test(mymatrix)
    pvalfisher = or$p.value
    message1 = paste(in_gezogen, "% vs. ", in_bk, "% Enrichment:",
                     enr, "OR (95%CI) =", signif(or$estimate, 3), paste0("(",
                                                                         signif(or$conf.int[1], 3), "-", signif(or$conf.int[2]),
                                                                         ")"), sep = " ")
    message2 = paste("p hypergeomtrisch=", signif(pval, 3),
                     "p fisher", signif(pvalfisher, 3))
    message3 = paste(aa, "in", aa + cc, "gezogenen vs.", aa +
                       bb, "in", aa + bb + cc + dd, "(grundgesamtheit)", sep = " ")
    message(message1)
    message(message2)
    message(message3)
    res = list(in_gezogen = in_gezogen, in_bk = in_bk, enrichment = enr,
               pval = pval, pval_fisher = pvalfisher, or = or$estimate,
               or_lower = or$conf.int[1], or_upper = or$conf.int[2],
               matrix = mymatrix, messages = c(message1, message2,
                                               message3), compactresult = data.frame(in_gezogen = in_gezogen,
                                                                                     in_bk = in_bk, enrichment = enr, pval = pval, pval_fisher = pvalfisher,
                                                                                     or = or$estimate, or_lower = or$conf.int[1], or_upper = or$conf.int[2],
                                                                                     Bes_Gez = mymatrix[1], Bes_nichtGez = mymatrix[3],
                                                                                     nichtBes_Gez = mymatrix[2], nichtBes_nichtGez = mymatrix[4],
                                                                                     matrix = paste(mymatrix, collapse = ", "), row.names = NULL))
    res
  }
