


## interaction test see http://www.bmj.com/content/326/7382/219.long
interactionTest  = function(mean1, se1, mean2, se2) {
  meandiff_se = sqrt(se1^2 + se2^2)
  meandiff = mean2 - mean1
  meandiff_cilow = meandiff - 1.96*meandiff_se
  meandiff_cihigh = meandiff + 1.96*meandiff_se
  meandiff_z = meandiff/meandiff_se
  meandiff_p = pnorm(abs(meandiff_z), lower.tail = F)*2
  if(meandiff_p>1) meandiff_p = 1
  data.table::data.table(mean1, se1, mean2, se2, meandiff, meandiff_se, meandiff_cilow, meandiff_cihigh, meandiff_z, meandiff_p)

}
