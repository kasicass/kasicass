安装环境

```
$ python -m pip install pandas xlrd
```

运行

```
$ python xls2csv.py data.xls
```

用 gbk，保证生成的 .csv 用 excel 打开，中文不是乱码。

```python
sheet.to_csv('sheet1.csv', encoding='gbk')
```