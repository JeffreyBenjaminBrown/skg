FROM jeffreybbrown/hode:latest AS builder

WORKDIR /opt/skg
COPY Cargo.toml Cargo.lock /opt/skg/
COPY server /opt/skg/server
COPY tools/introspect/code-map /opt/skg/tools/introspect/code-map
COPY tools/introspect/call-graph /opt/skg/tools/introspect/call-graph
COPY tools/introspect/change-forest /opt/skg/tools/introspect/change-forest
COPY tools/scope-reducer /opt/skg/tools/scope-reducer
COPY tools/rust-field-tags /opt/skg/tools/rust-field-tags
COPY schema.tql /opt/skg/schema.tql
RUN cargo build --release --bin skg

FROM jeffreybbrown/hode:latest

WORKDIR /opt/skg
COPY --from=builder /opt/skg/target/release/skg /usr/local/bin/skg
COPY schema.tql /opt/skg/schema.tql
COPY windows/container/run-servers.sh /opt/skg/windows/container/run-servers.sh
COPY README.org /opt/skg/README.org
COPY glossary.md /opt/skg/glossary.md
COPY docs/setup.org /opt/skg/docs/setup.org
COPY docs/COMMANDS.org /opt/skg/docs/COMMANDS.org
COPY docs/merging.org /opt/skg/docs/merging.org
COPY docs/moving-nodes-across-repos.org /opt/skg/docs/moving-nodes-across-repos.org
COPY docs/hyperlinks.md /opt/skg/docs/hyperlinks.md
COPY docs/forks.md /opt/skg/docs/forks.md
RUN chmod +x /usr/local/bin/skg /opt/skg/windows/container/run-servers.sh

ENV SKG_PROJECT_ROOT=/opt/skg
ENV PATH="/usr/local/bin:${PATH}"
EXPOSE 1731/tcp

CMD ["sleep", "infinity"]
