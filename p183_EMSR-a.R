#���޲��ء�������˹, �����������Ż�. 2008, ������: �й��������ó�����. 382.��p183����7.5EMSR-a�㷨
EMSRa = function(Fare = Fare, Mean = Mean, sd = sd, p_up = numeric(length(Fare)), cap = cap) {
  N_FProd <- length(Fare)
  # ASSURE CONSISTENCY OF THE INPUTS
  tmp <- sort.int(Fare, decreasing=T, method = "sh", index.return=TRUE)
  Fare <- tmp$x
  ind <- tmp$ix
  Mean <- Mean[ind]
  sd <- sd[ind]
  p_up <- p_up[ind]
  # INITIALIZE PROTECTION LEVELS
  p <- vector(mode="numeric", length(Fare)-1)
  for (i in (length(Fare):2)) {
    for(j in 1:(i-1)){
      y <- qnorm((1 - Fare[i]/Fare[j]), mean=Mean[j], sd=sd[j])
      p[i-1]=p[i-1]+y
    }
  } # end for i

  return(p)
} # end function


Fare <- c(1050,950,699,520)
Mean <- c(17.3,45.1,39.6,34.0)
sd <- c(5.8,15.0,13.2,11.3)    #qnorm��������Ҫ����sd����
cap <- 130
p <- round(EMSRa(Fare = Fare, Mean = Mean, sd = sd, cap = cap),1)
cat("1-N������ˮƽ�ֱ��ǣ�",p)
