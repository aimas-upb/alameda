FROM python:3-alpine

RUN mkdir -p /usr/src/app

RUN mkdir -p /usr/src/app/data/input_dir
RUN mkdir -p /usr/src/app/data/output_dir

WORKDIR /usr/src/app

COPY requirements.txt /usr/src/app/

RUN pip3 install --no-cache-dir -r requirements.txt

COPY . /usr/src/app

EXPOSE 8080

ENTRYPOINT ["python3"]

CMD ["-m", "server"]