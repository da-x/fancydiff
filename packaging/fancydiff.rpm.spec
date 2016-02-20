Name:           @@PKG_NAME@@
Version:        @@PKG_VERSION@@
Release:        @@PKG_RELEASE@@%{?dist}
Summary:        Fancy coloring of diffs for Git
Group:          System Environment/Development Tools
License:        GPLv2
URL:            https://github.com/da-x/fancydiff
Source0:        %{name}-%{version}.tar.gz
Source1:        Version.hs
Source2:        stack-work-downloads.tar.gz
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:  stackage-dist-@@RESOLVER@@-downloads
BuildRequires:  stackage-dist-@@RESOLVER@@-stack
BuildRequires:  stackage-dist-@@RESOLVER@@-indices

BuildRequires:  git
BuildRequires:  ncurses-devel
BuildRequires:  libicu-devel
BuildRequires:  zlib-devel
BuildRequires:  openssl-devel
BuildRequires:  gmp-devel
BuildRequires:  pcre-devel

%description
Fancy coloring of diffs for Git

%global debug_package %{nil}

%define stackage_dist() \
    set +o posix \
    source %{_prefix}/%{_lib}/stackage-dist-@@RESOLVER@@/helpers.sh \
    stackage-dist-%{1} @@RESOLVER@@ $(pwd) \

%prep
%setup -q

ln -s %{_sourcedir}/stack-work-downloads.tar.gz .
%stackage_dist setup

rm version.sh
cp %{_sourcedir}/Version.hs app/Internal/Version.hs

%build
%stackage_dist env

stack build

%install
%stackage_dist env

mkdir -p $RPM_BUILD_ROOT/%{_datadir}/licences/%{name}
install LICENSE $RPM_BUILD_ROOT/%{_datadir}/licences/%{name}/LICENSE

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
install $(stack exec -- which fancydiff) $RPM_BUILD_ROOT/%{_prefix}/bin/%name

%files

%{_prefix}/bin/%name
%{_datadir}/licences/%{name}/LICENSE

%changelog
