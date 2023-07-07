# icfpc2023

2023-07-07T21:00: JSTより

# Links

* Contest 2023 page http://icfpcontest2023.github.io/
* Twitter https://twitter.com/icfpcontest2023
* Discord https://discord.com/invite/b9Zuy9D2


# API スクリプトの実行

プロジェクトルートで `token.txt` にトークン文字列を書いておいてください。

* 注意: 実行には以下のコマンドが必要です。適宜インストールしてください。

- curl
- jq
- ghc - runghc コマンド使います

## API スクリプト: サブミット実行

```shell
./api-sh/submit.sh PROBLEM_ID JSON_FILENAME
```

## API スクリプト: 問題のダウンロード

以下のコマンドを実行すると `problems/` 以下に全問題がダウンロードされます。
すでに 1-45 までの問題はダウンロード済です。 今後追加問題が出た場合などは、`api-sh/get-problem.sh` を参考にしてください。

```shell
./api-sh/save-problems.sh
```
