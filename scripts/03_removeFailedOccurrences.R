library(data.table)
library(dplyr)

df <- fread("outputs/uncleanedOccs.csv") %>%
  mutate(colid = 1:nrow(.))

df <- df %>%
  filter(decimalLongitude != 0 & decimalLatitude != 0) %>%
  filter(.cap != FALSE)
# let's first start by filtering data on things where some flagged points need to be kepts
#'Abarema_callejasii.png
spp_df <- df %>%
  filter(validName == "Abarema callejasii")

#Acacia_betchei.png
spp_df <- df %>%
  filter(validName == "Acacia betchei")

ptr_part1 <- spp_df %>%
  filter(decimalLatitude < -40)

ptr_part2 <- spp_df %>%
  filter(.cap == FALSE)

ptr1 <- rbind(ptr_part1, ptr_part2) # ptr stands for points to remove

#Acacia_biflora.png
spp_df <- df %>%
  filter(validName == "Acacia biflora")

ptr2 <- spp_df %>%
  filter(decimalLongitude > 130)

#Acacia_falcata.png
spp_df <- df %>%
  filter(validName == "Acacia falcata")

ptr_part1 <- spp_df %>%
  filter(decimalLongitude > 155)

ptr_part2 <- spp_df %>%
  filter(decimalLongitude < 143)

ptr_part3 <- spp_df %>%
  filter(.cap == FALSE)

ptr3 <- rbind(ptr_part1, ptr_part2, ptr_part3)

rm(ptr_part1, ptr_part2, ptr_part3)

#Acacia_falciformis.png
spp_df <- df %>%
  filter(validName == "Acacia falciformis")

ptr_part1 <- spp_df %>%
  filter(decimalLongitude > 155)

ptr_part2 <- spp_df %>%
  filter(decimalLongitude < 140)

ptr4 <- rbind(ptr_part1, ptr_part2)

rm(ptr_part1, ptr_part2)

#Acacia_flocktoniae.png
spp_df <- df %>%
  filter(validName == "Acacia flocktoniae")

ptr5 <- spp_df %>%
  filter(decimalLongitude < 140)
#Acacia_heterochroa.png
spp_df <- df %>%
  filter(validName == "Acacia heterochroa")

ptr6 <- spp_df %>%
  filter(decimalLongitude > 140)

#Acacia_leioderma.png
spp_df <- df %>%
  filter(validName == "Acacia leioderma")

ptr7 <- spp_df %>%
  filter(decimalLongitude < 50 | decimalLongitude >140)

#Acacia_leprosa.png
spp_df <- df %>%
  filter(validName == "Acacia leprosa")

ptr8 <- spp_df %>%
  filter(decimalLongitude < 140 | decimalLongitude > 155)

#Acacia_lunata.png
spp_df <- df %>%
  filter(validName == "Acacia lunata")

ptr9 <- spp_df %>%
  filter(decimalLongitude < 140 | .cap == FALSE)

#Acacia_myrtifolia.png
spp_df <- df %>%
  filter(validName == "Acacia myrtifolia")

ptr10 <- spp_df %>%
  filter(decimalLongitude < 100 | .cap == FALSE)

#Acacia_olgana.png
# removed with .cap != false script


#Acacia_pycnostachya.png
spp_df <- df %>%
  filter(validName == "Acacia pycnostachya")

ptr11 <- spp_df %>%
  filter(decimalLongitude < 140)

#Acacia_sclerophylla.png
spp_df <- df %>%
  filter(validName == "Acacia sclerophylla")

ptr12 <- spp_df %>%
  filter(decimalLatitude > 30)

#Acacia_senegal.png
spp_df <- df %>%
  filter(validName == "Acacia senegal")

ptr13 <- spp_df %>%
  filter(decimalLongitude < -50 | decimalLongitude > 100)

#Acacia_translucens.png
spp_df <- df %>%
  filter(validName == "Acacia translucens")

ptr14 <- spp_df %>%
  filter(decimalLatitude > 0 | decimalLongitude > 140)

#Albizia_amara.png
spp_df <- df %>%
  filter(validName == "Albizia amara")

ptr15 <- spp_df %>%
  filter(decimalLongitude < 0)

#Anadenanthera_peregrina.png
spp_df <- df %>%
  filter(validName == "Anadenanthera peregrina")

ptr16 <- spp_df %>%
  filter(decimalLongitude > -30)

#Archidendron_lucyi.png

spp_df <- df %>%
  filter(validName == "Archidendron lucyi")

ptr17 <- spp_df %>%
  filter(decimalLongitude < 30)

#Calliandra_brenesii.png
# removed with 0,0 filter

#Calliandra_brevipes.png
spp_df <- df %>%
  filter(validName == "Calliandra brevipes")

ptr18 <- spp_df %>%
  filter(decimalLongitude > 30)

#Calliandra_chilensis.png
spp_df <- df %>%
  filter(validName == "Calliandra chilensis")

ptr19 <- spp_df %>%
  filter(decimalLongitude > -60)

#Calliandra_viscidula.png
spp_df <- df %>%
  filter(validName == "Calliandra viscidula")

ptr20 <- spp_df %>%
  filter(decimalLongitude > -20)

#Calliandropsis_nervosus.png
spp_df <- df %>%
  filter(validName == "Calliandropsis nervosus")

ptr21 <- spp_df %>%
  filter(decimalLatitude > 24)

#Cathormion_umbellatum.png
# removed with 0,0 filter

#Chloroleucon_foliolosum.png
spp_df <- df %>%
  filter(validName == "Chloroleucon foliolosum")

ptr22 <- spp_df %>%
  filter(decimalLongitude > -20)

#Chloroleucon_mangense.png
# removed with 0,0 filter

#Cojoba_arborea.png
# removed with 0,0 filter

#Entada_africana.png
#removed with 0,0 filter

#Entada_mannii.png
#removed with 0,0 filter

#Gagnebina_pterocarpa.png
spp_df <- df %>%
  filter(validName == "Gagnebina pterocarpa")

ptr23 <- spp_df %>%
  filter(decimalLongitude < -20)

#Inga_tomentosa.png
spp_df <- df %>%
  filter(validName == "Inga tomentosa")

ptr24 <- spp_df %>%
  filter(decimalLongitude > -55)

#Leucaena_cuspidata.png
spp_df <- df %>%
  filter(validName == "Leucaena cuspidata")

ptr25 <- spp_df %>%
  filter(decimalLongitude > -20)

#Leucaena_diversifolia.png
spp_df <- df %>%
  filter(validName == "Leucaena diversifolia")

ptr26 <- spp_df %>%
  filter(decimalLongitude > -78 | decimalLongitude < -150 | decimalLatitude < 0)

rm(ptr_part1)

#Mimosa_bahamensis.png
spp_df <- df %>%
  filter(validName == "Mimosa bahamensis")

ptr27 <- spp_df %>%
  filter(decimalLongitude > -60)

#Mimosa_hilariana.png
spp_df <- df %>%
  filter(validName == "Mimosa hilariana")

ptr28 <- spp_df %>%
  filter(decimalLatitude > -15)

#Mimosa_lanuginosa.png
spp_df <- df %>%
  filter(validName == "Mimosa lanuginosa")

ptr28 <- spp_df %>%
  filter(decimalLatitude > -15)

#Mimosa_leptocarpa.png
spp_df <- df %>%
  filter(validName == "Mimosa leptocarpa")

ptr29 <- spp_df %>%
  filter(decimalLatitude < -5)

#Mimosa_mollis.png
spp_df <- df %>%
  filter(validName == "Mimosa mollis")

ptr30 <- spp_df %>%
  filter(decimalLatitude < 0)

#Mimosa_paludosa.png
spp_df <- df %>%
  filter(validName == "Mimosa paludosa")

ptr31 <- spp_df %>%
  filter(decimalLongitude < -80 | .cen == FALSE)

#Mimosa_polycarpa.png
spp_df <- df %>%
  filter(validName == "Mimosa polycarpa")

ptr32 <- spp_df %>%
  filter(decimalLongitude < -100 |
           decimalLongitude > -40 | .cen == FALSE)

#Mimosa_strigillosa.png
# fixed with 0,0 filter

#Mimosa_tenuiflora.png
spp_df <- df %>%
  filter(validName == "Mimosa tenuiflora")

ptr_part1 <- spp_df %>%
  filter(decimalLongitude < -65 & decimalLatitude < 0)

ptr_part2 <- spp_df %>%
  filter(decimalLongitude > -25 | decimalLatitude < -20)

ptr33 <- rbind(ptr_part1, ptr_part2)

rm(ptr_part1, ptr_part2)

#Neptunia_oleracea.png
spp_df <- df %>%
  filter(validName == "Neptunia oleracea")

ptr34 <- spp_df %>%
  filter(decimalLongitude < -120 | decimalLongitude > 130 | .cen == FALSE)

#Neptunia_pubescens.png
spp_df <- df %>%
  filter(validName == "Neptunia pubescens")

ptr35 <- spp_df %>%
  filter(.cen == FALSE)

#Newtonia_buchananii.png
# removed by 0,0

#Pararchidendron_pruinosum.png
spp_df <- df %>%
  filter(validName == "Pararchidendron pruinosum")

ptr36 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < -50)

#Piptadenia_flava.png
spp_df <- df %>%
  filter(validName == "Pararchidendron pruinosum")

ptr36 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < -50)

#Piptadenia_obliqua.png
# removed with 0,0

#Piptadenia_robusta.png
# pass, no problems

#Xylia_xylocarpa.png
spp_df <- df %>%
  filter(validName == "Xylia xylocarpa")

ptr37 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < 20 | decimalLongitude > 120)

#Zapoteca_portoricensis.png
spp_df <- df %>%
  filter(validName == "Zapoteca portoricensis")

ptr38 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude > -25 | decimalLongitude < -120)

## now to the other filter
#Acacia_catechu.png
spp_df <- df %>%
  filter(validName == "Acacia catechu")

ptr39 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < 0 | decimalLongitude > 120)

#Acacia_cyclops.png
spp_df <- df %>%
  filter(validName == "Acacia cyclops")

ptr40 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < 100 | decimalLongitude > 150)

#Acacia_decurrens.png
spp_df <- df %>%
  filter(validName == "Acacia decurrens")

ptr_part1 <- spp_df %>%
  filter(.cen == FALSE | decimalLongitude < 120 | decimalLongitude > 155)

ptr_part2 <- spp_df %>%
  filter(decimalLatitude > 1)

ptr41 <- rbind(ptr_part1, ptr_part2) %>%
  distinct(colid, .keep_all = T)

rm(ptr_part1, ptr_part2)

#Acacia_eburnea.png
spp_df <- df %>%
  filter(validName == "Acacia eburnea")

ptr42 <- spp_df %>%
  filter(decimalLongitude> 60)

#Acacia_farnesiana.png
spp_df <- df %>%
  filter(validName == "Acacia farnesiana")

ptr43 <- spp_df %>%
  filter(decimalLongitude < -130 | decimalLongitude > -33)

#Acacia_julifera.png
spp_df <- df %>%
  filter(validName == "Acacia julifera")

ptr44 <- spp_df %>%
  filter(decimalLongitude < 140 | decimalLatitude < -30)


#Acacia_longifolia.png
spp_df <- df %>%
  filter(validName == "Acacia longifolia")

ptr45 <- spp_df %>%
  filter(decimalLongitude < 120 | decimalLongitude > 170)

#Acacia_mangium.png
spp_df <- df %>%
  filter(validName == "Acacia mangium")

ptr46 <- spp_df %>%
  filter(decimalLongitude < 137 | decimalLatitude < -30)

#Acacia_melanoxylon.png
spp_df <- df %>%
  filter(validName == "Acacia melanoxylon")

ptr47 <- spp_df %>%
  filter(decimalLongitude < 120 | decimalLongitude > 170|
           decimalLatitude > 0)

#Acacia_nilotica.png
spp_df <- df %>%
  filter(validName == "Acacia nilotica")

ptr48 <- spp_df %>%
  filter(decimalLongitude < -30 | decimalLongitude > 100)

#Acacia_peninsularis.png
spp_df <- df %>%
  filter(validName == "Acacia peninsularis")

ptr49 <- spp_df %>%
  filter(decimalLongitude  > -90)

#Acacia_pennata.png
spp_df <- df %>%
  filter(validName == "Acacia pennata")

ptr50 <- spp_df %>%
  filter(decimalLongitude  < 50)

#Acacia_pharangites.png
spp_df <- df %>%
  filter(validName == "Acacia pharangites")

ptr51 <- spp_df %>%
  filter(decimalLongitude  > 140)

#Acacia_podalyriifolia.png
spp_df <- df %>%
  filter(validName == "Acacia podalyriifolia")

ptr52 <- spp_df %>%
  filter(decimalLongitude  < 142 | decimalLongitude > 170)

#Acacia_pycnantha.png
spp_df <- df %>%
  filter(validName == "Acacia podalyriifolia")

ptr52 <- spp_df %>%
  filter(decimalLongitude  < 135 | decimalLongitude > 170 | decimalLatitude > 0)

#Acacia_redolens.png
spp_df <- df %>%
  filter(validName == "Acacia redolens")

ptr53 <- spp_df %>%
  filter(decimalLongitude  < 100 | decimalLongitude > 145)

#Acacia_retinodes.png
spp_df <- df %>%
  filter(validName == "Acacia retinodes")

ptr54 <- spp_df %>%
  filter(decimalLongitude  < 120 | decimalLongitude > 170 | decimalLatitude > -20)

#Acacia_saligna.png
spp_df <- df %>%
  filter(validName == "Acacia saligna")

ptr55 <- spp_df %>%
  filter(decimalLongitude  < 100 | decimalLongitude > 130)

#Acaciella glauca
spp_df <- df %>%
  filter(validName == "Acaciella glauca")

ptr56 <- spp_df %>%
  filter(decimalLongitude  < 120 | decimalLongitude > -5)

#Adenanthera_pavonina.png
spp_df <- df %>%
  filter(validName == "Adenanthera pavonina")

ptr57 <- spp_df %>%
  filter(decimalLongitude  < 60 | .inst == FALSE)

#Albizia_adianthifolia.png
spp_df <- df %>%
  filter(validName == "Albizia adianthifolia")

ptr58 <- spp_df %>%
  filter(decimalLongitude  < -35)

#Albizia_chinensis.png
spp_df <- df %>%
  filter(validName == "Albizia chinensis")

ptr59 <- spp_df %>%
  filter(decimalLongitude  < 60)

#Albizia_julibrissin.png
spp_df <- df %>%
  filter(validName == "Albizia julibrissin")

ptr60 <- spp_df %>%
  filter(decimalLongitude  < 42 | decimalLatitude < 5)


#Albizia_saman.png
spp_df <- df %>%
  filter(validName == "Albizia saman")

ptr61 <- spp_df %>%
  filter(decimalLatitude  < -5 | decimalLatitude > 18 |
           decimalLongitude > -60 | decimalLongitude < -120)

#Calliandra_biflora.png.

spp_df <- df %>%
  filter(validName == "Calliandra biflora")

ptr62 <- spp_df %>%
  filter(decimalLatitude  > 40)

#Calliandra_haematocephala.png
spp_df <- df %>%
  filter(validName == "Calliandra haematocephala")

ptr63 <- spp_df %>%
  filter(decimalLatitude  < -15 |
           decimalLongitude > -61)

#Calliandra_surinamensis.png
spp_df <- df %>%
  filter(validName == "Calliandra surinamensis")

ptr64 <- spp_df %>%
  filter(decimalLatitude  > 11.5 |
           decimalLongitude < -83 | decimalLongitude > -34)

#Desmanthus_virgatus.png
spp_df <- df %>%
  filter(validName == "Desmanthus virgatus")

ptr_part1 <- spp_df %>%
  filter(decimalLongitude < -84.5 | decimalLongitude > -34.5)
ptr_part2 <- spp_df %>%
  filter(decimalLatitude < -60)
ptr_part3 <- spp_df %>%
  filter(decimalLatitude > -20 & decimalLatitude < 20 & decimalLongitude < -60)

ptr65 <- rbind(ptr_part1, ptr_part2, ptr_part3) %>%
  distinct(colid, .keep_all = T)

rm(ptr_part1, ptr_part2, ptr_part3)

#Entada_abyssinica.png
spp_df <- df %>%
  filter(validName == "Entada abyssinica")

ptr66 <- spp_df %>%
  filter(decimalLongitude < -20)

#Falcataria_moluccana.png
spp_df <- df %>%
  filter(validName == "Falcataria moluccana")

ptr67 <- spp_df %>%
  filter(decimalLongitude < 93 | decimalLongitude < -14)

#Inga_edulis.png
spp_df <- df %>%
  filter(validName == "Inga edulis")

ptr68 <- spp_df %>%
  filter(decimalLatitude > 12.5 |
           decimalLongitude > -34.5 | decimalLongitude < -115)

#Mimosa_diplotricha.png
spp_df <- df %>%
  filter(validName == "Mimosa diplotricha")

ptr69 <- spp_df %>%
  filter(decimalLongitude > -34.5 | decimalLongitude < -120)

#Mimosa_invisa.png
spp_df <- df %>%
  filter(validName == "Mimosa invisa")

ptr70 <- spp_df %>%
  filter(decimalLongitude > -34.5 | decimalLongitude < -77)

#Mimosa_pigra.png
spp_df <- df %>%
  filter(validName == "Mimosa pigra")

ptr71 <- spp_df %>%
  filter(decimalLongitude > -34.5)

#Mimosa_pudica.png
spp_df <- df %>%
  filter(validName == "Mimosa pudica")

ptr72 <- spp_df %>%
  filter(decimalLongitude > -34.5 | decimalLongitude < -118 |
           decimalLatitude > 24)

#Mimosa_somnians.png
spp_df <- df %>%
  filter(validName == "Mimosa somnians")

ptr73 <- spp_df %>%
  filter(decimalLongitude > -34.5)


# Paraserianthes_lophantha.png
spp_df <- df %>%
  filter(validName == "Paraserianthes lophantha")

ptr74 <- spp_df %>%
  filter(decimalLongitude > 130 | decimalLongitude < 60)

# Pithecellobium_dulce.png
spp_df <- df %>%
  filter(validName == "Pithecellobium dulce")

ptr_part1 <-  spp_df %>%
  filter(decimalLongitude < -120 | decimalLongitude > -58)

ptr_part2 <-  spp_df %>%
  filter(decimalLongitude > -98 & decimalLatitude > 24)

ptr75 <- rbind(ptr_part1, ptr_part2) %>%
  distinct(colid, .keep_all = T)

rm(ptr_part1, ptr_part2)

# Pithecellobium_hymenaeafolium.png
spp_df <- df %>%
  filter(validName == "Pithecellobium hymenaeafolium")

ptr76 <- spp_df %>%
  filter(decimalLongitude > -20)

# Prosopis_chilensis.png

spp_df <- df %>%
  filter(validName == "Prosopis chilensis")

ptr77 <- spp_df %>%
  filter(decimalLongitude > -50 | decimalLatitude > 0)

# Prosopis_glandulosa.png
spp_df <- df %>%
  filter(validName == "Prosopis glandulosa")

ptr78 <- spp_df %>%
  filter(decimalLongitude < -130 | decimalLongitude > -86)

# Prosopis_globosa.png

spp_df <- df %>%
  filter(validName == "Prosopis globosa")

ptr79 <- spp_df %>%
  filter(decimalLongitude < -90)

# Prosopis_juliflora.png
spp_df <- df %>%
  filter(validName == "Prosopis juliflora")

ptr80 <- spp_df %>%
  filter(decimalLongitude < -120 | decimalLongitude > -60 |
           decimalLatitude < -20)

# Prosopis_pallida.png
spp_df <- df %>%
  filter(validName == "Prosopis pallida")

ptr81 <- spp_df %>%
  filter(decimalLongitude < -100 | decimalLongitude > -60 |
           decimalLatitude > 0)

# Prosopis_reptans.png
spp_df <- df %>%
  filter(validName == "Prosopis reptans")

ptr82 <- spp_df %>%
  filter(decimalLatitude > 0)

# Vachellia_cornigera.png
spp_df <- df %>%
  filter(validName == "Vachellia cornigera")

ptr_part1 <- spp_df %>%
  filter(decimalLatitude > 20 & decimalLongitude > -86.5)

ptr_part2 <- spp_df %>%
  filter(decimalLongitude > -80)

ptr83 <- rbind(ptr_part1, ptr_part2)

## what else is left? let's check the annotation session outcome
# Acacia auriculiformis
spp_df <- df %>%
  filter(validName == "Acacia auriculiformis")

ptr84 <- spp_df %>%
  filter(decimalLongitude < 137.5 | decimalLatitude < -21.5 | decimalLongitude > 153.5)


# rbind all of the ptr dfs together

t_ptr <- paste0("ptr", 1:84) %>% lapply(get) %>% bind_rows %>% distinct()

df$colid <- as.character(df$colid)
t_ptr$colid <- as.character(t_ptr$colid)

# keep colid that are not in t_ptr (total points to remove)
tdf <- df %>%
  filter(!colid %in% t_ptr$colid)

tdf

# now read in my df stating process
csv <- read.csv("outputs/testsess.csv")
library(stringr)
csv <- csv %>%
  mutate(validName = stringr::word(file, start = 1, end = 1, sep = fixed('.')))
csv <- csv %>%
  mutate(validName = stringr::str_replace(validName, pattern = "_", replacement = " "))

tdf_lj <- left_join(tdf, csv)


# more points to remove
mptr <- tdf_lj %>%
  filter(Pass == "Fail" & Remove_flaggedPoints == "Yes" & .summary == FALSE)


final_df <- tdf_lj %>%
  filter(!colid %in% mptr$colid) %>%
  select(id, decimalLongitude, decimalLatitude, year, scientificName,
         genus, species, locality, validName)


write.csv(x = final_df,
          file = "outputs/cleanedOccs.csv",
          row.names = F)
