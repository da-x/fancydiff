
class Fancydiff < Formula
  desc "Fancy coloring of diffs for Git"
  homepage "http://github.com/da-x/fancydiff"
  url "http://github.com/da-x/fancydiff/releases/download/v0.2.3/fancydiff-0.2.3-0001.g7df0a81-darwin-x86_64.tar.gz"
  sha256 "186d7165d1792f00d4f0ce5e638abe9161e28c3935e7382494b32dcc4fee99fb"
  version "0.2.3"

  depends_on :arch => :x86_64

  bottle :unneeded

  def install
    doc.install Dir["doc/*", "README.md", "LICENSE.txt"]
    libexec.install Dir["bin", "exe", "deps"]
    bin.write_exec_script Dir["#{libexec}/bin/*"]
  end
end

