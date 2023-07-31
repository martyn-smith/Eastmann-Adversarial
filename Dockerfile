FROM ubuntu:22.04
ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt update && apt install -y curl gfortran make pandoc python3.10-full texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra
RUN curl -sSL https://install.python-poetry.org | POETRY_HOME=/ python3 -

#RUN export PATH="~/.local/bin:$PATH"
COPY src ./src
COPY env ./env
COPY pyproject.toml .
COPY Makefile .
