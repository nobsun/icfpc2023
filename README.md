# ICFP Programming Contest 2023

This is Team Sampou's repository for the [ICFP Programming Contest 2023](http://icfpcontest2023.github.io/).

## Programming Languages used

* Haskell
* Python
* Shell Script

## Members

* [Katsutoshi Itoh](https://github.com/cutsea110)
* [Kei Hibino](https://github.com/khibino)
* [Masahiro Sakai](https://github.com/msakai)
* [Nobuo Yamashita](https://github.com/nobsun)
* [Yasuyuki Ogawa](https://github.com/oganet)

## Instruction for members

2023-07-07T21:00: JSTより

* username: Team Sampou

### Links

* Contest 2023 page http://icfpcontest2023.github.io/
* Twitter https://twitter.com/icfpcontest2023
* Discord https://discord.com/invite/b9Zuy9D2

### ライブラリのインストール

Ubuntuの場合:

```bash
sudo apt-get install liblbfgsb-dev libblas-dev liblapack-dev
```

https://hackage.haskell.org/package/numeric-optimization-0.1.1.0#installing-prerequisites も参照

### API スクリプトの実行

プロジェクトルートで `token.txt` にトークン文字列を書いておいてください。

* 注意: 実行には以下のコマンドが必要です。適宜インストールしてください。

- curl
- jq
- ghc - runghc コマンド使います

#### API スクリプト: サブミット実行

```shell
./api-sh/submit.sh PROBLEM_ID JSON_FILENAME
```

#### API スクリプト: 問題のダウンロード

以下のコマンドを実行すると `problems/` 以下に全問題がダウンロードされます。
すでに 1-45 までの問題はダウンロード済です。 今後追加問題が出た場合などは、`api-sh/get-problem.sh` を参考にしてください。

```shell
./api-sh/save-problems.sh
```
