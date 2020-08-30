FROM archlinux
WORKDIR /home/student/
RUN useradd -m student
# nice languages (and utils etc)
RUN pacman -Syu base-devel wget unzip gnucobol swi-prolog racket lua53 go flex bison rust git glu glut mariadb python vim emacs nano --noconfirm
ARG user=makepkg
RUN useradd -m $user
RUN echo "$user ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/$user
USER $user
WORKDIR /home/$user
# LUA
RUN sudo ln -s /bin/lua5.3 /bin/lua \
  && wget https://luarocks.org/releases/luarocks-3.3.1.tar.gz \
  && tar zxpf luarocks-3.3.1.tar.gz \
  && cd luarocks-3.3.1 && ./configure && make && sudo make install
# J
RUN curl http://www.jsoftware.com/download/j901/install/j901_linux64.tar.gz --output j901_linux64.tar.gz \
  && tar -xzf j901_linux64.tar.gz \
  && cd j901/bin \
  && sudo ./install-usr.sh \
  && sudo cp jconsole /usr/bin/jconsole \
  && sudo chmod 755 /usr/bin/jconsole \
  && sudo cp jhs /usr/bin/jhs \
  && sudo chmod 755 /usr/bin/jhs \
  && echo -e "load 'pacman'\n'install' jpkg '*'\nexit 0" | sudo ijconsole \
  && cd .. \
  && rm -rf j901 j901_linux64.tar.gz \
  && echo -e "config_jhs_=: 3 : 0\nPORT=: 65001\nUSER=: 'student'\nPASS=: 'password'\n)\nload'~addons/ide/jhs/core.ijs'\ninit_jhs_''" | sudo tee /usr/share/j/9.01/addons/ide/jhs/config/jhs.cfg \
  && echo -e "#!/bin/bash\necho \"load'~addons/ide/jhs/config/jhs.cfg'\" | jconsole" | sudo tee -a /usr/bin/jhs-c \
  && sudo chmod 755 /usr/bin/jhs-c
# jhs
EXPOSE 65001
# yay languages 
RUN git clone https://aur.archlinux.org/yay.git \
  && cd yay \
  && makepkg -sri --needed --noconfirm \
  && cd \
  && rm -rf .cache yay \
  && yay -Sy befungee-git --noconfirm \
  && yay -S gforth --noconfirm
USER root
RUN userdel -r $user \
  && echo 'PS1="[\u@\h \w] "' >> /home/student/.bashrc \
  && chown -R student /home/student \
  && echo "student ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/student
USER student
WORKDIR /home/student/
# local packages and such
RUN luarocks path >> /home/student/.bashrc \
  && sudo luarocks install lcurses \
  && go get github.com/google/uuid
