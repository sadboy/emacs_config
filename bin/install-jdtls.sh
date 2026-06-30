#!/bin/bash

# Install Eclipse jdtls on Ubuntu or MacOS (Homebrew)

set -euo pipefail

JDTLS_VERSION="${JDTLS_VERSION:-latest}"
JDTLS_INSTALL_DIR="${JDTLS_INSTALL_DIR:-${HOME}/.local/share/jdtls}"
JDTLS_BASE_URL="https://download.eclipse.org/jdtls/snapshots"

err() {
    echo "Error: $*" >&2
    exit 1
}

info() {
    echo ">>> $*"
}

detect_os() {
    case "$(uname -s)" in
        Linux*)  echo "linux"  ;;
        Darwin*) echo "macos"  ;;
        *)       err "Unsupported operating system: $(uname -s)" ;;
    esac
}

ensure_java() {
    if command -v java >/dev/null 2>&1; then
        local version
        version="$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')"
        local major
        major="$(echo "${version}" | awk -F '.' '{ if ($1 == 1) print $2; else print $1 }')"
        if [ "${major}" -ge 17 ] 2>/dev/null; then
            info "Found compatible Java ${version}"
            return 0
        fi
        info "Java ${version} found but jdtls requires Java 17+; installing JDK 17."
    else
        info "Java not found; installing JDK 17."
    fi

    local os
    os="$(detect_os)"
    case "${os}" in
        linux)
            sudo apt-get update
            sudo apt-get install -y openjdk-17-jdk
            ;;
        macos)
            command -v brew >/dev/null 2>&1 || err "Homebrew is required on macOS."
            brew install openjdk@17
            ;;
    esac
}

install_deps() {
    local os
    os="$(detect_os)"
    case "${os}" in
        linux)
            command -v curl >/dev/null 2>&1 || sudo apt-get install -y curl
            command -v tar  >/dev/null 2>&1 || sudo apt-get install -y tar
            ;;
        macos)
            command -v brew >/dev/null 2>&1 || err "Homebrew is required on macOS."
            command -v curl >/dev/null 2>&1 || brew install curl
            ;;
    esac
}

resolve_download_url() {
    if [ "${JDTLS_VERSION}" = "latest" ]; then
        local latest
        latest="$(curl -fsSL "${JDTLS_BASE_URL}/latest.txt")" \
            || err "Failed to resolve latest jdtls version."
        echo "${JDTLS_BASE_URL}/${latest}"
    else
        echo "${JDTLS_BASE_URL}/jdt-language-server-${JDTLS_VERSION}.tar.gz"
    fi
}

download_and_extract() {
    local url="$1"
    local tmpdir
    tmpdir="$(mktemp -d)"
    trap "rm -rf \"${tmpdir}\"" RETURN

    info "Downloading jdtls from ${url}"
    curl -fSL "${url}" -o "${tmpdir}/jdtls.tar.gz" \
        || err "Failed to download jdtls archive."

    info "Extracting to ${JDTLS_INSTALL_DIR}"
    rm -rf "${JDTLS_INSTALL_DIR}"
    mkdir -p "${JDTLS_INSTALL_DIR}"
    tar -xzf "${tmpdir}/jdtls.tar.gz" -C "${JDTLS_INSTALL_DIR}" \
        || err "Failed to extract jdtls archive."
}

create_launcher() {
    local bindir="${HOME}/.local/bin"
    local launcher="${bindir}/jdtls"
    mkdir -p "${bindir}"

    local config
    case "$(detect_os)" in
        linux) config="config_linux" ;;
        macos) config="config_mac"   ;;
    esac

    local launcher_jar
    launcher_jar="$(find "${JDTLS_INSTALL_DIR}/plugins" \
        -name 'org.eclipse.equinox.launcher_*.jar' | head -n1)"
    [ -n "${launcher_jar}" ] || err "Could not locate Equinox launcher jar."

    cat > "${launcher}" <<EOF
#!/bin/bash
# Auto-generated jdtls launcher

JDTLS_HOME="${JDTLS_INSTALL_DIR}"
WORKSPACE="\${1:-\${HOME}/.cache/jdtls-workspace}"

exec java \\
    -Declipse.application=org.eclipse.jdt.ls.core.id1 \\
    -Dosgi.bundles.defaultStartLevel=4 \\
    -Declipse.product=org.eclipse.jdt.ls.core.product \\
    -Dlog.level=ALL \\
    -Xmx1G \\
    --add-modules=ALL-SYSTEM \\
    --add-opens java.base/java.util=ALL-UNNAMED \\
    --add-opens java.base/java.lang=ALL-UNNAMED \\
    -jar "${launcher_jar}" \\
    -configuration "\${JDTLS_HOME}/${config}" \\
    -data "\${WORKSPACE}"
EOF

    chmod +x "${launcher}"
    info "Launcher installed at ${launcher}"

    case ":${PATH}:" in
        *":${bindir}:"*) ;;
        *) info "Note: ${bindir} is not on your PATH; add it to use 'jdtls' directly." ;;
    esac
}

main() {
    info "Detected OS: $(detect_os)"
    install_deps
    ensure_java

    local url
    url="$(resolve_download_url)"
    download_and_extract "${url}"
    create_launcher

    info "jdtls installation complete: ${JDTLS_INSTALL_DIR}"
}

main "$@"
