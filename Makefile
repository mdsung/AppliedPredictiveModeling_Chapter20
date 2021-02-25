figure/figure_20_3.%: code/figure_20_3.R\
	code/figure_20_3.R

figure/figure_20_6.%: code/figure_20_6.R\
	code/figure_20_6.R

figure/figure_20_8.pdf figure/figure_20_8.png: code/read_two_class_dataset.R\
											code/figure_20_8.R
	code/figure_20_8.R

figure/figure_20_9.pdf figure/figure_20_9.png: code/read_two_class_dataset.R\
											code/figure_20_9.R
	code/figure_20_9.R

presentation/APM20.html: figure/figure_20_3.png\
						figure/figure_20_6.png\
						figure/figure_20_8.png\
						figure/figure_20_9.png\
						presentation/APM20.Rmd
	R -e 'library(rmarkdown); library(here); render(here("presentation/APM20.Rmd"))'