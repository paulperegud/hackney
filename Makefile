PROJECT = hackney

# ERLC_OPTS = +debug_info +'{parse_transform, lager_transform}'
REBAR_DEPS_DIR = $(shell pwd)/deps
export REBAR_DEPS_DIR

DEPS = idna ssl_verify_hostname
dep_idna = git https://github.com/benoitc/erlang-idna 1.0.2
dep_ssl_verify_hostname = git https://github.com/deadtrickster/ssl_verify_hostname.erl 1.0.5

include erlang.mk # defines: app deps tests clean rel
