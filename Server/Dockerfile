FROM erlang

COPY rebar.config /midas_server/
COPY apps /midas_server/apps/
COPY config/* /midas_server/config/

EXPOSE 21237

RUN cd /midas_server/ \
	&& rebar3 as prod release

VOLUME /midas

CMD /midas_server/_build/prod/rel/midas_server/bin/midas_server -noshell