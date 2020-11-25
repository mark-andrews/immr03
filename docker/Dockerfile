FROM xmjandrews/verse:20.07.15

RUN install2.r --error cowplot \
                       latex2exp \
                       ellipse \
                       ggrepel \
                       WWGbook \
                       here

RUN rm -rf /tmp/downloaded_packages /tmp/*.rds

RUN apt-get update
RUN apt-get install -y libglpk-dev libglpk40 
 
RUN apt-get install -y texlive-xetex \
                       texlive-plain-generic \
                       texlive-latex-extra

RUN sudo apt-get update && \
	sudo apt-get install -y --no-install-recommends \
	texlive-fonts-recommended \
  texlive-fonts-extra \
  texlive-bibtex-extra \
  texlive-plain-generic
