

fa.asreml <- function(object, uniplot = F, uniplot.tol = 0.85, uniplot.cex = 0.5,
                      trunc.char = c(1, 6), blups = TRUE, regplot = F, addedplot = F,
                      g.list = NULL, heatmap = F, heatmap.ord = "asis", agnes.method = "average") {
  fa.var <- function(fat) {
    unlist(lapply(fat, function(x) {
      if (x$Fun == "fa") {
        x$FacNam
      } else {
        NULL
      }
    }))
  }
  fa.outer <- function(fat) {
    unlist(lapply(fat, function(x) {
      if (x$Fun == "fa") {
        x$Obj
      } else {
        NULL
      }
    }))
  }
  fa.inner.name <- function(fat) {
    unlist(lapply(fat, function(x) {
      if (x$Fun != "fa") {
        x$FacNam
      } else {
        NULL
      }
    }))
  }
  fa.inner.var <- function(fat) {
    unlist(lapply(fat, function(x) {
      if (x$Fun != "fa") {
        x$Obj
      } else {
        NULL
      }
    }))
  }
  fa.inner.fun <- function(fat) {
    unlist(lapply(fat, function(x) {
      if (x$Fun != "fa") {
        x$Fun
      } else {
        NULL
      }
    }))
  }
  ide.order <- function(fat) {
    ide <- which(unlist(lapply(fat, function(x) {
      as.logical(sum(unlist(lapply(x, function(y) {
        if (y$Fun == "ide") TRUE else FALSE
      }))))
    })))
    if (length(ide) > 0) {
      c(seq(along = fat)[-ide], ide)
    } else {
      seq(along = fat)
    }
  }
  if (!inherits(object, "asreml")) {
    stop("\nObject must be of class 'asreml'\n")
  }
  if (!requireNamespace("asreml", quietly = TRUE)) {
    stop("Requires package 'asreml'")
  }
  tt <- attr(object$mf, "model.terms")$random$Terms.obj
  if (length(which.fun <- attr(tt, "specials")$fa) == 0) {
    stop("No fa() term in model\n")
  }
  ttf <- attr(tt, "factors")
  tt.vars <- dimnames(ttf)[[1]]
  term.labels <- attr(tt, "term.labels")
  which.fa <- list()
  names.fa <- NULL
  for (i in which.fun) {
    x <- which(ttf[i, ] > 0)
    for (j in x) {
      which.fa <- c(which.fa, list(which(ttf[, j] > 0)))
      names.fa <- c(names.fa, term.labels[j])
    }
  }
  nterms <- length(which.fa)
  faterms <- vector(mode = "list", length = nterms)
  for (w in 1:nterms) {
    ww <- which.fa[[w]]
    faterms[[w]] <- lapply(
      attr(object$mf, "model.terms")$random$Vars[tt.vars[ww]],
      function(x) {
        y <- list(x$Fun, x$FacNam, x$Obj)
        names(y) <- c("Fun", "FacNam", "Obj")
        y
      }
    )
  }
  names(faterms) <- names.fa
  idx <- ide.order(faterms)
  nice.sum <- summary(object, vparameters = TRUE)$vparameters
  if (is.null(mf <- object$mf)) {
    if (is.null(object$RDS)) {
      stop(object, "is missing model frame and has no RDS component.")
    } else {
      mf <- readRDS(object$RDS)
    }
  }
  if (length(y <- attr(mf, "traits")$lhs) > 1) {
    stop("No method for multivariate")
  }
  y <- mf[[y]]
  total.where <- nterms
  gammas.lst <- list()
  fanam <- names(faterms)
  uniplot.lst <- blup.lst <- regplot.lst <- addedplot.lst <- heatmap.lst <- agnes.lst <- NULL
  if (uniplot) {
    uniplot.lst <- list()
  }
  if (blups) {
    blup.lst <- list()
  }
  if (regplot) {
    regplot.lst <- list()
  }
  if (addedplot) {
    addedplot.lst <- list()
  }
  if (heatmap) {
    heatmap.lst <- list()
    if (heatmap.ord == "cluster") {
      agnes.lst <- list()
    }
  }
  for (nt in idx) {
    if (length(fa.inner.var(faterms[[nt]])) > 1) {
      stop("Only first order interaction with FA term allowed\n")
    }
    Variety <- factor(as.character(mf[[fa.inner.var(faterms[[nt]])]]))
    Site <- mf[[fa.outer(faterms[[nt]])]]
    outer.name <- fa.outer(faterms[[nt]])
    inner.name <- fa.inner.var(faterms[[nt]])
    snam <- levels(Site)
    ll <- min(nchar(snam))
    if (length(trunc.char)) {
      sn <- substring(snam, min(ll, trunc.char[1]), max(
        ll,
        trunc.char[2]
      ))
    } else {
      sn <- snam
    }
    if (length(unique(sn)) < length(snam)) {
      stop(paste(
        "Fewer levels in FA term than expected,\n",
        " set 'trunc.char' to avoid non-unique level names in factor",
        outer.name
      ))
    }
    vnam <- levels(Variety)
    ns <- length(snam)
    nv <- length(vnam)
    if (ns == 0) {
      stop(paste("\n", fa.outer(faterms[[nt]]), " is not a factor\n",
        sep = ""
      ))
    }
    if (nv == 0) {
      stop(paste("\n", fa.inner.var(faterms[[nt]]), " is not a factor\n",
        sep = ""
      ))
    }
    Variety.fac <- Variety
    if (length(grep(":", unique(Variety.fac)))) {
      stop(paste("Levels of", inner.name, "cannot contain the ':' character"))
    }
    if (length(grep(":", unique(Site)))) {
      stop(paste("Levels of", outer.name, "cannot contain the ':' character"))
    }
    gammas <- matrix(nice.sum[[names(faterms)[nt]]], nrow = ns)
    k <- ncol(gammas) - 1
    psi <- gammas[, 1]
    names(psi) <- snam
    Lam <- gammas[, -1, drop = FALSE]
    if (k > 1) {
      ss <- svd(Lam)
      Lam <- -Lam %*% ss$v
    }
    dimnames(Lam) <- list(snam, paste("fac", 1:k, sep = "_"))
    Gmat <- Lam %*% t(Lam) + diag(psi)
    Cmat <- cov2cor(Gmat)
    paf.site <- matrix(0, nrow = ns, ncol = k)
    dimnames(paf.site) <- list(snam, paste("fac", 1:k, sep = "_"))
    for (i in 1:k) {
      paf.site[, i] <- 100 * diag(Lam[, i] %*% t(Lam[
        ,
        i
      ])) / diag(Gmat)
    }
    if (k > 1) {
      all <- 100 * diag(Lam %*% t(Lam)) / diag(Gmat)
      paf.site <- cbind(paf.site, all)
    }
    paf.mod <- 100 * sum(diag(Lam %*% t(Lam))) / sum(diag(Gmat))
    dd <- 1 / sqrt(diag(Gmat))
    Lamc <- diag(dd) %*% Lam
    dimnames(Lamc) <- dimnames(Lam)
    gammas.lst[[nt]] <- list(
      Gmat = Gmat, Cmat = Cmat, `site %vaf` = paf.site,
      `total %vaf` = paf.mod, `rotated loads` = Lam, `specific var` = psi,
      `rotated loads - c` = Lamc
    )
    if (k == 1) {
      uniplot <- FALSE
    }
    if (uniplot) {
      bp.main <- names(faterms)[nt]
      if (k == 2) {
        uniplot.lst[[nt]] <- lattice::xyplot(Lamc[, 1] ~ Lamc[
          ,
          2
        ],
        xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
        asp = "s", xlab = "Loading 2", ylab = "Loading 1",
        main = bp.main, panel = function(x, y, subscripts,
                                         sn, tol, lcex, ...) {
          panel.curve(sqrt(1 - x^2), from = -1, to = 1)
          panel.curve(-sqrt(1 - x^2), from = -1, to = 1)
          ne <- length(sn)
          radius <- sqrt(x * x + y * y)
          lcol <- rep("blue", ne)
          ltyp <- rep(1, ne)
          llwd <- rep(1.5, ne)
          lcol[radius < tol] <- "red"
          ltyp[radius < tol] <- 3
          llwd[radius < tol] <- 1
          panel.segments(rep(0, ne), rep(0, ne), x,
            y,
            lty = ltyp, col = lcol, lwd = llwd
          )
          ltext(x, y, sn,
            cex = lcex, col = lcol,
            srt = 45
          )
        }, sn = sn, tol = uniplot.tol, lcex = uniplot.cex
        )
      } else {
        tmp <- vector(mode = "list", length = k * (k -
          1) / 2)
        natmp <- character(0)
        for (i in seq(1, k - 1)) {
          for (j in seq(i + 1, k)) {
            natmp <- c(natmp, paste("Ld", i, j, sep = ""))
            tmp[[(i - 1) * (k - 1) + (j - i)]] <- lattice::xyplot(Lamc[
              ,
              i
            ] ~ Lamc[, j],
            xlim = c(-1.1, 1.1), ylim = c(
              -1.1,
              1.1
            ), asp = "s", xlab = paste(
              "Loading",
              j
            ), ylab = paste("Loading", i), main = bp.main,
            panel = function(x, y, subscripts, sn,
                             tol, lcex, ...) {
              panel.curve(sqrt(1 - x^2),
                from = -1,
                to = 1
              )
              panel.curve(-sqrt(1 - x^2),
                from = -1,
                to = 1
              )
              ne <- length(sn)
              radius <- sqrt(x * x + y * y)
              lcol <- rep("blue", ne)
              ltyp <- rep(1, ne)
              llwd <- rep(1.5, ne)
              lcol[radius < tol] <- "red"
              ltyp[radius < tol] <- 3
              llwd[radius < tol] <- 1
              panel.segments(rep(0, ne), rep(0, ne),
                x, y,
                lty = ltyp, col = lcol, lwd = llwd
              )
              ltext(x, y, sn,
                cex = lcex, col = lcol,
                srt = 45
              )
            }, sn = sn, tol = uniplot.tol, lcex = uniplot.cex
            )
          }
        }
        names(tmp) <- natmp
        uniplot.lst[[nt]] <- tmp
      }
    }
    if (heatmap) {
      dimnames(Cmat) <- list(sn, sn)
      if (heatmap.ord == "asis") {
        sn.ord <- sn
      } else if (heatmap.ord == "cluster") {
        dis.mat <- 1 - Cmat
        agnes.lst[[nt]] <- agnes(
          x = dis.mat, diss = TRUE,
          method = agnes.method
        )
        sn.ord <- agnes.lst[[nt]]$order.lab
      } else {
        sn.ord <- heatmap.ord
      }
      Cmat.ord <- Cmat[rev(sn.ord), rev(sn.ord)]
      diag(Cmat.ord) <- NA
      range(Cmat.ord, na.rm = T)
      atss <- seq(-1, 1, 0.1)
      hh <- rev(rainbow(256, start = 0, end = 2 / 3))
      hm.lab <- fa.outer(faterms[[nt]])
      hm.main <- names(faterms)[nt]
      heatmap.lst[[nt]] <- levelplot(Cmat.ord[
        sn.ord,
        rev(sn.ord)
      ],
      at = atss, col.regions = hh, scales = list(x = list(
        rot = 60,
        cex = 0.9
      ), y = list(cex = 0.9)), xlab = hm.lab,
      ylab = hm.lab, main = hm.main
      )
    }
    if (blups) {
      cc <- coef(object, list = TRUE)[[names(faterms)[nt]]]
      blup.df <- data.frame(blup = as.vector(cc))
      nn <- dimnames(cc)[[1]]
      temp <- strsplit(nn, split = ":", fixed = TRUE)
      tt.inner <- sapply(temp, function(x) x[2])
      tt.outer <- sapply(temp, function(x) x[1])
      tt.outer <- sapply(strsplit(tt.outer,
        split = "_",
        fixed = T
      ), function(x) paste(x[-1], collapse = "_"))
      tt.inner <- sapply(strsplit(tt.inner,
        split = "_",
        fixed = T
      ), function(x) paste(x[-1], collapse = "_"))
      # tt.inner <- sapply(strsplit(tt.inner, split = "_",
      #                             fixed = T), function(x) paste(x[-c(1,2)], collapse = "_"))
      blup.df[[outer.name]] <- tt.outer
      blup.df[[inner.name]] <- tt.inner
      score.df <- subset(blup.df, is.element(
        blup.df[[outer.name]],
        paste("Comp", 1:k, sep = "")
      ))
      score.mat <- matrix(score.df$blup, ncol = k)
      if (k > 1) {
        score.mat <- -score.mat %*% ss$v
      }
      score.df$blupr <- as.vector(score.mat)
      blup.df <- subset(blup.df, !is.element(
        blup.df[[outer.name]],
        paste("Comp", 1:k, sep = "")
      ))
      blup.df$regblup <- as.vector(score.mat %*% t(Lam))
      blup.inmet.df <- subset(blup.df, is.element(
        blup.df[[inner.name]],
        unique(Variety.fac)
      ))
      score.inmet.df <- subset(score.df, is.element(
        score.df[[inner.name]],
        unique(Variety.fac)
      ))
      blup.inmet.df <- blup.inmet.df[order(
        type.convert(blup.inmet.df[[outer.name]],
          as.is = FALSE
        ),
        type.convert(blup.inmet.df[[inner.name]],
          as.is = FALSE
        )
      ), ]
      pres <- tapply(y, list(Variety, Site), function(x) length(x[!is.na(x)]))
      blup.inmet.df$pres <- as.vector(pres)
      score.inmet.df <- score.inmet.df[order(
        type.convert(score.inmet.df[[outer.name]],
          as.is = FALSE
        ),
        type.convert(score.inmet.df[[inner.name]],
          as.is = FALSE
        )
      ), ]
      blup.lst[[nt]] <- list(
        blups = blup.df, scores = score.df,
        blups.inmet = blup.inmet.df, scores.inmet = score.inmet.df
      )
    }
    if (regplot) {
      if (!blups) {
        stop("'blups' must be set to TRUE for regplots\n")
      }
      score.mat <- matrix(score.inmet.df$blupr, ncol = k)
      dimnames(score.mat) <- list(
        unique(score.inmet.df[[inner.name]]),
        paste("fac", 1:k, sep = "_")
      )
      if (is.null(g.list)) {
        g.list <- list()
        dd <- dimnames(score.mat)[[1]]
        for (kk in 1:k) {
          g.list[[kk]] <- dd[order(score.mat[, kk])][1]
          g.list[[kk + k]] <- dd[order(-score.mat[
            ,
            kk
          ])][1]
        }
        g.list <- unlist(g.list)
        g.list <- unique(g.list)
        if (length(g.list) != 2 * k) {
          tt <- table(Variety)
          tt <- tt[!is.element(names(tt), g.list)]
          tt <- names(tt)[order(-tt)]
          tt <- tt[1:(2 * k - length(g.list))]
          g.list <- c(g.list, tt)
        }
      }
      score.mat <- score.mat[sort(g.list), , drop = FALSE]
      regplot.df <- subset(blup.inmet.df, is.element(
        blup.inmet.df[[inner.name]],
        g.list
      ))
      for (kk in 1:k) {
        facnam <- paste("fac", kk, sep = ".")
        regplot.df[[facnam]] <- rep(Lam[, kk], each = length(g.list))
      }
      regplot.df[[inner.name]] <- factor(regplot.df[[inner.name]])
      xform <- paste(paste("fac", 1:k, sep = "."), collapse = "+")
      xform <- formula(paste("blup~", xform, "|", inner.name,
        sep = ""
      ))
      rp.main <- paste(names(faterms)[nt], "BLUPS")
      regplot.lst[[nt]] <- lattice::xyplot(xform,
        data = regplot.df,
        outer = TRUE, as.table = TRUE, par.strip.text = list(cex = 0.6),
        main = rp.main, panel = function(x, y, slopes,
                                         ...) {
          panel.xyplot(x, y)
          panel.abline(b = slopes[
            current.column(),
            current.row()
          ], a = 0)
        }, slopes = score.mat
      )
      if (addedplot) {
        Yadd <- list()
        for (kk in 1:k) {
          Lamk <- Lam
          Lamk[, kk] <- 0
          Yadd[[kk]] <- score.mat %*% t(Lamk)
        }
        xform <- paste(paste("fac", 1:k, sep = "."),
          collapse = "+"
        )
        xform <- formula(paste("blup~", xform, "|",
          inner.name,
          sep = ""
        ))
        ap.main <- paste(names(faterms)[nt], "BLUPS")
        addedplot.lst[[nt]] <- lattice::xyplot(xform,
          data = regplot.df,
          outer = T, as.table = T, par.strip.text = list(cex = 0.6),
          main = ap.main, panel = function(x, y, subscripts,
                                           slopes, yadj, ...) {
            yadj <- yadj[[current.row()]]
            yadj <- yadj[current.column(), ]
            panel.xyplot(x, y - yadj)
            panel.abline(a = 0, b = slopes[
              current.column(),
              current.row()
            ])
          }, slopes = score.mat, yadj = Yadd
        )
      }
    }
    ide.term <- (fa.inner.fun(faterms[[nt]]) == "ide")
    if (ide.term && nterms > 1) {
      ped.term <- character(0)
      for (ped in names(faterms)[-nt]) {
        if (fa.outer(faterms[[ped]]) == fa.outer(faterms[[nt]]) &&
          fa.inner.var(faterms[[ped]]) == fa.inner.var(faterms[[nt]]) &&
          fa.inner.fun(faterms[[ped]]) == "vm") {
          ped.term <- match(ped, names(faterms))
        }
      }
      if (length(ped.term) > 0) {
        fanam <- c(fanam, paste(fa.outer(faterms[[nt]]),
          ":", fa.inner.var(faterms[[nt]]), "-total",
          sep = ""
        ))
        total.where <- total.where + 1
        Gmat <- gammas.lst[[nt]]$Gmat + gammas.lst[[ped.term]]$Gmat
        Cmat <- cov2cor(Gmat)
        gammas.lst[[total.where]] <- list(
          Gmat = Gmat,
          Cmat = Cmat
        )
        if (blups) {
          blup.inmet <- data.frame(
            Site = blup.lst[[nt]]$blups.inmet[[outer.name]],
            Variety = blup.lst[[nt]]$blups.inmet[[inner.name]],
            pres = blup.lst[[nt]]$blups.inmet$pres,
            blup = blup.lst[[nt]]$blups.inmet$blup +
              blup.lst[[ped.term]]$blups.inmet$blup,
            regblup = blup.lst[[nt]]$blups.inmet$regblup +
              blup.lst[[ped.term]]$blups.inmet$regblup
          )
          names(blup.inmet) <- c(
            outer.name, inner.name,
            "pres", "blup", "regblup"
          )
          blup.lst[[total.where]] <- list(blups.inmet = blup.inmet)
        }
        if (heatmap) {
          dimnames(Cmat) <- list(sn, sn)
          if (heatmap.ord == "asis") {
            sn.ord <- sn
          } else if (heatmap.ord == "cluster") {
            dis.mat <- 1 - Cmat
            agnes.lst[[total.where]] <- agnes(
              x = dis.mat,
              diss = TRUE, method = agnes.method
            )
            sn.ord <- agnes.lst[[total.where]]$order.lab
          } else {
            sn.ord <- heatmap.ord
          }
          Cmat.ord <- Cmat[rev(sn.ord), rev(sn.ord)]
          diag(Cmat.ord) <- NA
          range(Cmat.ord, na.rm = T)
          atss <- seq(-1, 1, 0.1)
          hh <- rev(rainbow(256, start = 0, end = 2 / 3))
          hm.main <- fanam[length(fanam)]
          heatmap.lst[[total.where]] <- levelplot(Cmat.ord[
            sn.ord,
            rev(sn.ord)
          ],
          at = atss, col.regions = hh,
          scales = list(
            x = list(rot = 60, cex = 0.9),
            y = list(cex = 0.9)
          ), xlab = hm.lab, ylab = hm.lab,
          main = hm.main
          )
        }
      }
    }
  }
  if (uniplot) {
    names(uniplot.lst) <- names(faterms)
  }
  if (regplot) {
    names(regplot.lst) <- names(faterms)
  }
  if (addedplot) {
    names(addedplot.lst) <- names(faterms)
  }
  names(gammas.lst) <- fanam
  if (heatmap) {
    names(heatmap.lst) <- fanam
    if (heatmap.ord == "cluster") {
      names(agnes.lst) <- fanam
    }
  }
  if (blups) {
    names(blup.lst) <- fanam
  }
  return(list(
    gammas = gammas.lst, blups = blup.lst, uniplots = uniplot.lst,
    regplots = regplot.lst, addedplots = addedplot.lst,
    heatmaps = heatmap.lst, agnes = agnes.lst
  ))
}


# Genotypic Variance ------------------------------------------------------

genVarChart <- function(model, height = "55%", top = "15%") {
  ASM <- fa.asreml(model, trunc.char = NULL)

  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  Vg <- round(diag(ASM$gammas[[1]]$Gmat), 2)
  Env.stats <- as.data.frame(Vg) %>% tibble::rownames_to_column("site")

  g1 <- Env.stats %>%
    echarts4r::e_charts(site) %>%
    echarts4r::e_bar(Vg, name = "Genotypic Variance") %>%
    echarts4r::e_title("Genotypic Variance", subtext = paste("By environment", "-", dimen, "Factors")) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_color("#115ca2") %>% # #28a745
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataView") %>%
    echarts4r::e_labels() %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 65, fontSize = 12, margin = 8)) %>% # rotate
    echarts4r::e_grid(height = height, top = top)

  return(g1)
}

# Scores ------------------------------------------------------------------



scoreChart <- function(model, x = "Comp1", y = "Comp2", plot = TRUE, gen_selected = NULL) {
  ASM <- fa.asreml(model, trunc.char = NULL)
  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  objt <- ASM$blups[[1]]$scores[, -1]
  env <- names(objt)[1]
  gen <- names(objt)[2]

  if (dimen == 1) {
    sc <- objt %>%
      tidyr::spread(key = .data[[env]], value = blupr) %>%
      dplyr::mutate(Score = sqrt(.data[[x]]^2 + 0), Comp1 = round(.data[[x]], 3), Comp2 = 0)
    y <- ""
  } else {
    sc <- objt %>%
      tidyr::spread(key = .data[[env]], value = blupr) %>%
      dplyr::mutate(Score = sqrt(.data[[x]]^2 + .data[[y]]^2), Comp1 = round(.data[[x]], 3), Comp2 = round(.data[[y]], 3))
    y <- paste("&", y)
  }


  names(sc)[1] <- "gen"
  my_scale <- function(x) scales::rescale(x, to = c(5, 30))

  if (plot) {
    g2 <- sc %>%
      echarts4r::e_charts(Comp1) %>%
      echarts4r::e_scatter(Comp2, Score, bind = gen, scale = my_scale) %>%
      echarts4r::e_title("Scores Factor Analytic", subtext = paste(x, y, "scores by genotype")) %>%
      echarts4r::e_visual_map(Score,
        scale = my_scale, color = c("#440154", "#21908C", "#FDE725") # c("#db4437","white","#4285f4")
      ) %>%
      echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_tooltip(formatter = htmlwidgets::JS("
              function(params){
                return('<strong>' + params.name +
                       '</strong><br />x : ' + params.value[0] +
                       '<br />y : ' + params.value[1])
                        }
                  ")) %>%
      echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
      echarts4r::e_toolbox_feature(feature = "dataView") %>%
      echarts4r::e_toolbox_feature(feature = "dataZoom")

    if (!is.null(gen_selected)) {
      if (!gen_selected %in% sc$gen) {
        return(g2)
      }

      inx <- sc[sc$gen == gen_selected, ]
      inx_xAxis <- inx$Comp1
      inx_yAxis <- inx$Comp2
      inx_value <- inx$gen
      point <- list(xAxis = inx_xAxis, yAxis = ifelse(dimen == 1, 0, inx_yAxis), value = inx_value)

      g2 <- g2 %>% echarts4r::e_mark_point(data = point)
    }

    return(g2)
  } else {
    return(sc)
  }
}

# Variance explained  -----------------------------------------------------

waterChart <- function(model) {
  ASM <- fa.asreml(model, trunc.char = NULL)

  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  perc <- ASM$gammas[[1]]$`total %vaf` / 100
  liquid <- data.frame(val = rep(perc, 2))
  g4 <- liquid %>%
    echarts4r::e_charts() %>%
    echarts4r::e_liquid(val) %>%
    echarts4r::e_title("Percentage of Variance Explained", subtext = paste(dimen, "factors")) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataView")
  return(g4)
}


# Loadings ----------------------------------------------------------------


loadChart <- function(model, x = "fac_1", y = "fac_2", plot = TRUE, trial_selected = NULL) {
  ASM <- fa.asreml(model, trunc.char = NULL)
  env <- names(ASM$blups[[1]]$blups)[2]

  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  Vg <- round(diag(ASM$gammas[[1]]$Gmat), 2)
  Env.stats <- as.data.frame(Vg) %>% tibble::rownames_to_column("site")

  lstar <- ASM$gammas[[1]]$`rotated loads` %>%
    data.frame() %>%
    tibble::rownames_to_column("site")
  Env.stats <- merge(Env.stats, lstar, by = "site")

  clean.data <- model$mf %>%
    type.convert(as.is = FALSE) %>%
    data.frame()
  Env.means <- data.frame(site = levels(clean.data[, env]))

  env_effect <- try(coef(model)$fixed[paste0(env, "_", levels(clean.data[, env])), ], silent = T)
  if (class(env_effect) == "try-error") {
    env_effect <- try(coef(model)$rand[paste0(env, "_", levels(clean.data[, env])), ], silent = T)
    if (class(env_effect) == "try-error") env_effect <- 0
  }

  Env.means$coef <- env_effect
  Env.means$interc <- coef(model)$fixed["(Intercept)", ]

  Env.means$estima <- Env.means %>%
    dplyr::mutate(interc + coef) %>%
    dplyr::pull()
  names(Env.means)[4] <- "Environment"

  Env.stats <- merge(Env.stats, Env.means, by = "site")

  porc <- paste0(round(ASM$gammas[[1]]$`total %vaf`, 2), " %")

  if (dimen == 1) {
    Env.stats <- Env.stats %>%
      dplyr::mutate(
        Vg = round(Vg, 2),
        fac_1 = round(.data[[x]], 2),
        fac_2 = 0,
        Environment = round(Environment, 2),
        Vgs = round(e_scale(Vg), 2)
      ) %>%
      dplyr::arrange(Vg)
    y <- ""
  } else {
    Env.stats <- Env.stats %>%
      dplyr::mutate(
        Vg = round(Vg, 2),
        fac_1 = round(.data[[x]], 2),
        fac_2 = round(.data[[y]], 2),
        Environment = round(Environment, 2),
        Vgs = round(e_scale(Vg), 2)
      ) %>%
      dplyr::arrange(Vg)

    y <- paste("&", y)
  }

  if (!plot) {
    return(Env.stats)
  }


  my_scale <- function(x) scales::rescale(x, to = c(10, 50))
  g5 <- Env.stats %>%
    echarts4r::e_charts(fac_1) %>%
    echarts4r::e_scatter(fac_2, Environment, scale = my_scale, bind = site) %>%
    echarts4r::e_title(paste0("Loadings Factor Analytic ", porc), subtext = paste(x, y, "loads by environment")) %>%
    echarts4r::e_visual_map(Environment,
      scale = my_scale, color = c("#440154", "#21908C", "#FDE725") #  c("#db4437","white","#4285f4")
    ) %>%
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_tooltip(formatter = htmlwidgets::JS("
              function(params){
                return('<strong>' + params.name +
                       '</strong><br />fa1: ' + params.value[0] +
                       '<br />fa2: ' + params.value[1] +
                       '<br />BLUE: ' + params.value[2])
                        }
                  ")) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataView") %>%
    echarts4r::e_toolbox_feature(feature = "dataZoom")

  if (!is.null(trial_selected)) {
    if (!trial_selected %in% Env.stats$site) {
      return(g5)
    }

    inx <- Env.stats[Env.stats$site == trial_selected, ]
    inx_xAxis <- inx$fac_1
    inx_yAxis <- inx$fac_2
    inx_value <- inx$site
    point <- list(xAxis = inx_xAxis, yAxis = ifelse(dimen == 1, 0, inx_yAxis), value = inx_value)

    g5 <- g5 %>% echarts4r::e_mark_point(data = point)
  }

  return(g5)
}




# Variance explained by environment ---------------------------------------



varExpChart <- function(model, height = "65%", top = "15%") {
  ASM <- fa.asreml(model, trunc.char = NULL)

  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  Vg <- ASM$gammas[[1]]$`site %vaf` / 100
  Vg <- Vg %>%
    round(., 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("site")

  g6 <- Vg %>%
    echarts4r::e_charts(site) %>%
    echarts4r::e_bar(fac_1, name = "fa1", stack = "grp") %>%
    echarts4r::e_legend(show = T) %>%
    echarts4r::e_title("Variance Explained", subtext = paste("By environment", "-", dimen, "Factors")) %>%
    echarts4r::e_tooltip(formatter = e_tooltip_item_formatter("percent")) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_toolbox_feature(feature = "dataView") %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 65, fontSize = 12, margin = 8)) %>% # rotate
    echarts4r::e_grid(height = height, top = top)

  if (dimen >= 2) g6 <- g6 %>% echarts4r::e_bar(fac_2, name = "fa2", stack = "grp")
  if (dimen >= 3) g6 <- g6 %>% echarts4r::e_bar(fac_3, name = "fa3", stack = "grp")
  if (dimen >= 4) g6 <- g6 %>% echarts4r::e_bar(fac_4, name = "fa4", stack = "grp")

  return(g6)
}

# Correlation between environments ----------------------------------------


corrChart <- function(model, height = "65%", left = "20%") {
  ASM <- fa.asreml(model, trunc.char = NULL)

  CC <- round(ASM$gammas[[1]]$Cmat, 2)

  g3 <- CC %>%
    echarts4r::e_charts() %>%
    echarts4r::e_correlations(order = "hclust", visual_map = F) %>%
    echarts4r::e_tooltip() %>%
    echarts4r::e_visual_map(
      color = c("#4285f4", "white", "#db4437"),
      min = -1,
      max = 1,
      orient = "horizontal",
      left = "center",
      bottom = "bottom"
    ) %>%
    echarts4r::e_title("Correlation", "Between environments") %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = -45, fontSize = 12, margin = 8)) %>% # rotate
    echarts4r::e_grid(left = left, height = height)

  return(g3)
}


# Latent regression -------------------------------------------------------



latent_regress <- function(model, genotype, scale = T, text_labs = T, size_text = 1, alpha_text = 0.5, alpha_p = 0.5, return_data = F) {
  ASM <- fa.asreml(model, trunc.char = NULL)

  dimen <- ncol(ASM$gammas[[1]]$`rotated loads`)

  L.star <- ASM$gammas[[1]]$`rotated loads` %>%
    data.frame() %>%
    tibble::rownames_to_column("site")
  UgOve <- ASM$blups[[1]]$blups

  u <- min(UgOve$blup) + 0.10 * min(UgOve$blup)
  l <- max(UgOve$blup) + 0.10 * max(UgOve$blup)

  names(UgOve)[2:3] <- c("site", "gen")
  FAST <- merge(UgOve, L.star, by = "site") %>%
    tidyr::gather(., key = "loading", value = "value", -1, -2, -3, -4)

  SCores <- ASM$blups[[1]]$scores
  SCores[, 2] <- rep(paste0("fac_", 1:length(unique(SCores[, 2]))), each = nrow(SCores) / dimen)
  names(SCores)[2:3] <- c("Comp", "gen")
  SCores <- SCores %>% dplyr::select(-blup)

  FAST <- merge(FAST, SCores, by.y = c("Comp", "gen"), by.x = c("loading", "gen"))
  FAST$blup_wo_psi <- FAST$regblup # wo = without psi

  if (dimen == 2) {
    FAST[FAST$loading == "fac_2", "regblup"] <- FAST[FAST$loading == "fac_2", "regblup"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"]
  } else if (dimen == 3) {
    FAST[FAST$loading == "fac_2", "regblup"] <- FAST[FAST$loading == "fac_2", "regblup"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"]
    FAST[FAST$loading == "fac_3", "regblup"] <- FAST[FAST$loading == "fac_3", "regblup"] - FAST[FAST$loading == "fac_2", "value"] * FAST[FAST$loading == "fac_2", "blupr"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"]
  } else if (dimen == 4) {
    FAST[FAST$loading == "fac_2", "regblup"] <- FAST[FAST$loading == "fac_2", "regblup"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"]
    FAST[FAST$loading == "fac_3", "regblup"] <- FAST[FAST$loading == "fac_3", "regblup"] - FAST[FAST$loading == "fac_2", "value"] * FAST[FAST$loading == "fac_2", "blupr"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"]
    FAST[FAST$loading == "fac_4", "regblup"] <- FAST[FAST$loading == "fac_4", "regblup"] - FAST[FAST$loading == "fac_3", "value"] * FAST[FAST$loading == "fac_3", "blupr"] - FAST[FAST$loading == "fac_1", "value"] * FAST[FAST$loading == "fac_1", "blupr"] - FAST[FAST$loading == "fac_2", "value"] * FAST[FAST$loading == "fac_2", "blupr"]
  }

  if (return_data) {
    names(FAST) <- c("component", "genotype", "trial", "blup_psi", "reg_blup", "loading", "score", "blup_without_psi")
    FAST <- FAST[, c("component", "genotype", "trial", "blup_psi", "blup_without_psi", "reg_blup", "loading", "score")]
    return(FAST)
  }

  FAST <- FAST %>% subset(gen %in% c(genotype))

  p <- FAST %>%
    ggplot2::ggplot(aes(x = value, y = regblup, color = gen)) +
    ggplot2::geom_point(size = 3, alpha = alpha_p) +
    ggplot2::facet_grid(~loading, scales = "free_x") +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::geom_abline(aes(slope = blupr, intercept = 0, color = gen)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = " ", color = "Genotype")

  if (text_labs) {
    p <- p + geom_label_repel(ggplot2::aes(label = site),
      nudge_y = 0.05,
      nudge_x = -0.03,
      force = 1,
      size = size_text,
      alpha = alpha_text
    )
  }


  if (scale) p <- p + ggplot2::ylim(c(u, l))

  return(p)
}
