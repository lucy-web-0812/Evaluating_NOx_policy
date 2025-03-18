


grouped_data <- collated_data |> 
  filter(Directive != "NA") |>
  group_by(`Fuel`) 





# ----- Plot the data -------


ggplot(grouped_data) +
  geom_hline(yintercept = 0.01, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 0.1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 10, linewidth = 0.2, colour = "#636363") +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_x_continuous(name = "Power", trans= 'log10', 
                     labels = c("1 kW", "10 kW", "100 kW", "1 MW", "10 MW", "100 MW", "1 GW"),
                     breaks = c(1, 10,100,1000,10000,100000,1000000)) +
  scale_colour_manual(values = c(ecodesign_colour, road_transport_colour, plane_colour, ied_colour, ship_colour, mcp_colour, NRMM_colour)) +
  scale_y_continuous(trans = "log10",  breaks = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1,2,3, 5, 10), limits = c(0.01,10), 
                     name = expression(bold("NOx (g kWh"^{-1}*")"))) +
  scale_shape_manual(values = c(15,16,3,17,4,18,8)) +
  ggrepel::geom_text_repel(aes(x = `Power (kW)`, 
                               y = `NOx (mg/kWh)`/ 1000, 
                               label = str_wrap(str_replace_all(`Product`, "/", ",\n"), 10)), 
                           max.overlaps = 12, 
                           min.segment.length = 0.01, size = 1.65, 
                           segment.size = 0.2, segment.curvature = 0.15) +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  rsc_theme_full_spacing() +
  theme(legend.margin = margin(t = -10, r = 0, b = 0), 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 14), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_line(linewidth = 0.1, colour = "lightgrey")) +
  guides(colour = guide_legend(override.aes = list(size=4), nrow = 1),  shape = guide_legend(nrow = 1)) +
  annotation_logticks()



ggsave(filename = "full_graph_log_y.png", path = "plots/paper_plots/", device = "png", height = 171, width = 266, units = "mm")


# Now to make readable labels..... 



grouped_data_readble_labels <- grouped_data |> 
  mutate(Product = str_replace_all(Product, "-v", ""), 
         Product = str_replace_all(Product, "-c", "")) |> 
  distinct() |> 
  mutate(`Product` = ifelse(Product == "Drax", paste0(`Product`, " - ", `Fuel`), Product)) |> 
  mutate(`Product` = ifelse(Product == "RWE Total", paste0("RWE - ", `Fuel`), Product)) |> 
  mutate(`Product` = ifelse(Product == "Uniper Europe", paste0("Uniper - ", `Fuel`), Product)) |> 
  mutate(`Product` = ifelse(Directive == "NRMM" & is.na(Fuel) == F, paste0(Fuel, " ", Product), Product)) |> 
  # mutate(`Product` = ifelse(Product == "Light Commercial Vehicles 1760 - 3500 kg reference mass", paste0("Light Commercial Vehicles (1760 - 3500 kg) - ", Fuel), Product)) |> 
  # mutate(`Product` = ifelse(Product == "Light Commercial Vehicles 1,305 - 1760 kg reference mass", paste0("Light Commercial Vehicles (1305 - 1760 kg) - ", Fuel), Product)) |> 
  # mutate(`Product` = ifelse(Product == "Light Commercial Vehicles < 1,305kg reference mass", paste0("Light Commercial Vehicles (< 1305 kg) - ", Fuel), Product)) |> 
  # mutate(`Product` = ifelse(Product == "Passenger Vehicle", paste0("Car - ", Fuel), Product)) |> 
  # Now need to take some individual ones and call them a,b,c, etc.... 
  mutate(product_labels = Product) |> 
  mutate(product_labels = ifelse(Product == "Diesel Refrigerating units", "A", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Plate compactor/tampers/rammers ", "B", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Aerial lifts", "C", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Cement and mortar mixers - Small", "D", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Rollers (Modern)", "E", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Trenchers/mini excavators ", "F", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Skid steer loader", "G", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Pumps", "H", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Forest tractors/harvesters/skidders", "I", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Air/gas compressors ", "J", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "2SG Trimmers/strimmers/edgers/brush cutters ", "K", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "4SG Cement and mortar mixers - Large", "L", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Welders", "M", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Asphalt pavers/concrete pavers ", "N", product_labels)) |> 
  mutate(product_labels = ifelse(Product == "Diesel Excavators (wheel /crawler type) - Medium", "O", product_labels))
  



ggplot(grouped_data_readble_labels) +
  geom_hline(yintercept = 0.01, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 0.1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 10, linewidth = 0.2, colour = "#636363") +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_x_continuous(name = "Power", trans= 'log10', 
                     labels = c("1 kW", "10 kW", "100 kW", "1 MW", "10 MW", "100 MW", "1 GW"),
                     breaks = c(1, 10,100,1000,10000,100000,1000000)) +
  scale_colour_manual(values = c(ecodesign_colour, road_transport_colour, plane_colour, ied_colour, ship_colour, mcp_colour, NRMM_colour)) +
  scale_y_continuous(trans = "log10",  breaks = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1,2,3, 5, 10), limits = c(0.01,10), 
                     name = expression(bold("NOx (g kWh"^{-1}*")"))) +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_shape_manual(values = c(15,16,3,17,4,18,8)) +
  ggrepel::geom_text_repel(aes(x = `Power (kW)`, 
                               y = `NOx (mg/kWh)`/ 1000, 
                               label = str_wrap(str_replace_all(`product_labels`, "/", ",\n"), 12)), 
                           max.overlaps = 25, 
                           min.segment.length = 0.001, size = 2, 
                           segment.size = 0.2, segment.curvature = 0.25, force = 1.5) +
  
  rsc_theme_full_spacing() +
  theme(legend.margin = margin(t = -10, r = 0, b = 0), 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 14), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_line(linewidth = 0.1, colour = "lightgrey")) +
  guides(colour = guide_legend(override.aes = list(size=4), nrow = 1),  shape = guide_legend(nrow = 1)) +
  annotation_logticks()



ggsave(filename = "full_graph_log_y_readable_labels.png", path = "plots/paper_plots/", device = "png", height = 171, width = 266, units = "mm")



ggplotly_version <- ggplotly(
  ggplot(grouped_data_readble_labels) +
  geom_hline(yintercept = 0.01, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 0.1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 10, linewidth = 0.2, colour = "#636363") +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_x_continuous(name = "Power", trans= 'log10', 
                     labels = c("1 kW", "10 kW", "100 kW", "1 MW", "10 MW", "100 MW", "1 GW"),
                     breaks = c(1, 10,100,1000,10000,100000,1000000)) +
  scale_colour_manual(values = c(ecodesign_colour, road_transport_colour, plane_colour, ied_colour, ship_colour, mcp_colour, NRMM_colour)) +
  scale_y_continuous(trans = "log10",  breaks = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1,2,3, 5, 10), limits = c(0.01,10), 
                     name = "NOx (g kWh -1)") +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_shape_manual(values = c(15,16,3,17,4,18,8)) +
  rsc_theme_full_spacing() +
  theme(legend.margin = margin(t = -10, r = 0, b = 0), 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 14), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_line(linewidth = 0.1, colour = "lightgrey")) +
  guides(colour = guide_legend(override.aes = list(size=4), nrow = 1),  shape = guide_legend(nrow = 1)) +
  annotation_logticks())

