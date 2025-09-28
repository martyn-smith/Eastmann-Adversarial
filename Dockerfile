FROM ubuntu:22.04
ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt update && apt install -y curl gfortran git make pandoc python3.10-full texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra
RUN curl -sSL https://astral.sh/uv/install.sh | sh
RUN git clone https://github.com/martyn-smith/Eastmann-95 ./Eastmann-95
COPY pyproject.toml .
COPY env ./env
COPY Makefile .
# RUN uv pip install
