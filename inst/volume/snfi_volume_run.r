
# info from user
test_province <- c(1, 12, 'Cantabria')  # province code; alternatively, province name can be used
test_species <- c(21, 65, 'Pinus radiata')  # species code; alternatively, species name can be used
test_dbh <- c(20, 20, 20)  # in cm; be careful with the units! equations expect mm
test_dnm <- c(23, 23, 23)  # in cm; be careful with the units! equations expect mm
test_h <- c(15, 15, 15)  # in m
test_quality <- c('default', 1, 3)  # by default the best quality

source('snfi_support_functions.r')
source('snfi_volume_equations.r')

output <- silv_predict_snfi_volume(province = test_province,
                          species = test_species,
                          dbh = test_dbh,
                          h = test_h,
                          dnm = test_dnm,
                          quality = test_quality)
print(output)  # dm3

# Note: I have tried to avoid code interruption due to errors in individual inputs; when an error happens, it is notified in the console and NA is returned for that case.