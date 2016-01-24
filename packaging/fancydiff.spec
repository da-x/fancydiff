Name:           PKG_NAME
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        Fancy coloring of diffs for Git
Group:          System Environment/Development Tools
License:        GPLv2
URL:            https://github.com/da-x/fancydiff
Source0:        %{name}-%{version}.tar.gz
Source1:        Version.hs
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:  stack
BuildRequires:  ghc
BuildRequires:  git
BuildRequires:  libicu-devel
BuildRequires:  zlib-devel
BuildRequires:  openssl-devel
BuildRequires:  gmp-devel
BuildRequires:  pcre-devel

%description
Fancy coloring of diffs for Git

%global debug_package %{nil}

%prep
%setup -q

rm version.sh
cp %{_sourcedir}/Version.hs app/Internal/Version.hs
stack --no-terminal setup

%build
stack --no-terminal build

%install

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
cp .stack-work/install/*/*/*/bin/%name $RPM_BUILD_ROOT/%{_prefix}/bin/%name

%files
%{_prefix}/bin/%name

%changelog
