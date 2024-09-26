comp6 <- data.frame(comboGeneral(all.sp, m=6, freqs = 1))

comp5 <- data.frame(comboGeneral(all.sp, m=5, freqs = 1))
write.csv(comp5, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp5.csv", row.names = FALSE)

comp8 <- data.frame(comboGeneral(all.sp, m=8, freqs = 1))
write.csv(comp8, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp8.csv", row.names = FALSE)

comp10= data.frame(comboGeneral(all.sp, m=10, freqs = 1))
write.csv(comp10, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp10.csv", row.names = FALSE)

comp12= data.frame(comboGeneral(all.sp, m=12, freqs = 1))
write.csv(comp12, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp12.csv", row.names = FALSE)


comp11= data.frame(comboGeneral(all.sp, m=11, freqs = 1))
write.csv(comp11, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp11.csv", row.names = FALSE)


comp13= data.frame(comboGeneral(all.sp, m=13, freqs = 1))
write.csv(comp11, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp13.csv", row.names = FALSE)


comp7 <- data.frame(comboGeneral(all.sp, m=7, freqs = 1))
write.csv(comp7, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp7.csv", row.names = FALSE)



comp9 <- data.frame(comboGeneral(all.sp, m=9, freqs = 1))
write.csv(comp9, "analyses/interactions_v_traits/structural_coexistence/run_structural/comp9.csv", row.names = FALSE)
