
##環境化学物質 3 学会合同大会 自由集会2　2022.6.14
##なんでもできる！Rを使った環境データ解析事例
##GLMM（一般化線形混合モデル）でデータのばらつきに対応しよう！
##一般財団法人残留農薬研究所 石母田　誠
##使用ソフト：統計ソフトR ver. 4.1.0

``` r
####メソコスム試験（ダミーデータ）GLM解析###

toxicity<-read.table("GLMM-data.Ishimota.txt", header = T)	#データの読み込み
attach(toxicity)　　　　　　　　　　　　　　　　　　　　#データへのアクセス
pairs(toxicity)　　　　　　　　　　　　　　　　         #変数同士の関係性の図示
	
mode1 <-glm(Ind~EC+pred+PA+PB,family=poisson(link="log"),data=toxicity) 


summary(mode1)

```
186.23/19            #残差逸脱度(186.23)/残差自由度（19）：dispersion parameter

``` r
x2<-sum(residuals(mode1, type="pearson")^2)         　　　　#ピアソンのχ二乗統計量


x2/19         　　　　#ピアソンのχ二乗統計量(x2)/残差自由度（19）：dispersion parameter

```

``` r
####メソコスム試験（ダミーデータ）GLMM解析###

install.packages("lme4")                                    #lme4パッケージのインストール（お持ちの場合は割愛）

toxicity<-read.table("GLMM-data.Ishimota.txt", header = T)　#データの読み込み
attach(toxicity)

library(lme4)                                   　　　　#lme4のパッケージの読み込み

mode2 <-glmer(Ind~EC+pred+PA+PB+(1|id) ,family=poisson(link="log"),data=toxicity)    

                                               　　　　 #GLMM解析， +(1|id)：切片にランダム効果を追加

summary(mode2)
``` 
