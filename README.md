# Planner

```
██████╗ ██╗      █████╗ ███╗   ██╗███╗   ██╗███████╗██████╗
██╔══██╗██║     ██╔══██╗████╗  ██║████╗  ██║██╔════╝██╔══██╗
██████╔╝██║     ███████║██╔██╗ ██║██╔██╗ ██║█████╗  ██████╔╝
██╔═══╝ ██║     ██╔══██║██║╚██╗██║██║╚██╗██║██╔══╝  ██╔══██╗
██║     ███████╗██║  ██║██║ ╚████║██║ ╚████║███████╗██║  ██║
╚═╝     ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝
```

Todo をパース・編集するためのコマンドです。
プロジェクトの難しさ・重要度によってソートしつつ、todoを管理することを目的に開発しました。
現在haskellの開発環境をおかしくしてしまったのと、todo管理を[obsidian](https://obsidian.md/)に移行したので開発を停止しています。

https://github.com/neco8/Planner/issues/7

```
Commands:
  compile     compile command
  function    function command
```

---

## compile
テキストファイルをTodo形式にコンパイルします。

```
Options:
  -i FILE_PATH  --input=FILE_PATH                   input file path
  -o FILE_PATH  --output=FILE_PATH                  output file path
                --vscode                            vscode todo style
                --chart=FILE_PATH                   chart file path
                --work-suffix=FOR_WORK_FILE_SUFFIX  for work generation suffix
  -f FILE_PATH  --input-output=FILE_PATH            input and output file path
```

---

## function
標準入力で受け取った文字列を編集します。

```
Options:
  -t     --toggle-done                                     toggle done todo
         --adjust-done-day=HOW_MANY_DAYS_TO_ADJUST         adjust done-day back and forth
  -k     --habit-count-up                                  count up at habit
         --add-habit-tag=HOW_MANY_POINTS_UNTIL_HABIT_GOAL  start habit with goal
         --is-kanban-apm                                   is kanban style
```

---

# 使用した主なライブラリ

Megaparsec           - Todoのパース
microlens            - データ処理
optparse-declarative - コマンドライン処理

---

# 感想

elmの開発経験から、なるべく型とスマートコンストラクタを意識して開発しました。
ただ、今だにhaskellのアプリケーション設計についてはピンときていません。特に非同期的機能が入ってきたらどうすればいいのかが理解できていません。

microlensを初めて使い、簡単な使い方だけ学習できたのは良かったです。いずれは圏論的な知識も勉強してみたいですが……
ただ、todoのソートアルゴリズムが暗黙的になってしまったので、アルゴリズムの変更に弱いなと後悔してます。
