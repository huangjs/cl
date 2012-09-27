(with-chart (:pie 300 200)
  (add-slice "A" 5.0d0)
  (add-slice "B" 2.0d0)
  (save-file "minimal-pie-chart-vecto.png"))