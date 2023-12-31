import pandas
from plotnine import ggplot, aes, theme_bw, facet_wrap, geom_line, geom_point, labs

d_ori = pandas.read_csv('input/data-eg2.csv')
d = pandas.melt(d_ori, id_vars='Time', var_name='Item', value_name='Y')
d['Item'] = pandas.Categorical(d.Item, categories=['Weight', 'Bodyfat'])

d1 = d.query("(Item == 'Bodyfat' & Time <= 30) | (Item == 'Weight' & Time <= 20)")
d2 = d.query("(Item == 'Bodyfat' & Time >= 31 & Time <= 40) | (Item == 'Weight' & Time >= 31 & Time <= 40)")
d3 = d.query("(Item == 'Bodyfat' & Time >= 51) | (Item == 'Weight' & Time >= 51)")

d_weight_na1a = d.query("(Item == 'Weight' & 21 <= Time & Time <= 30)")
d_weight_na2a = d.query("(Item == 'Weight' & 20 <= Time & Time <= 31)")
d_weight_na1b = d.query("(Item == 'Weight' & 41 <= Time & Time <= 50)")
d_weight_na2b = d.query("(Item == 'Weight' & 40 <= Time & Time <= 51)")
d_bodyfat_na1 = d.query("(Item == 'Bodyfat' & 41 <= Time & Time <= 50)")
d_bodyfat_na2 = d.query("(Item == 'Bodyfat' & 40 <= Time & Time <= 51)")

p = (ggplot(d1, aes('Time', 'Y'))
    + theme_bw(base_size=18)
    + facet_wrap('~Item', ncol=1, scales='free_y')
    + geom_line()
    + geom_point(shape='o', size=2)
    + geom_line(d2, aes('Time', 'Y'))
    + geom_point(d2, aes('Time', 'Y'), shape='o', size=2)
    + geom_line(d3, aes('Time', 'Y'))
    + geom_point(d3, aes('Time', 'Y'), shape='o', size=2)
    + geom_line(d_weight_na2a, aes('Time', 'Y'), linetype='dashed', alpha=0.5)
    + geom_point(d_weight_na1a, aes('Time', 'Y'), shape='o', color='gray', size=2, alpha=0.5)
    + geom_line(d_weight_na2b, aes('Time', 'Y'), linetype='dashed', alpha=0.5)
    + geom_point(d_weight_na1b, aes('Time', 'Y'), shape='o', color='gray', size=2, alpha=0.5)
    + geom_line(d_bodyfat_na2, aes('Time', 'Y'), linetype='dashed', alpha=0.5)
    + geom_point(d_bodyfat_na1, aes('Time', 'Y'), shape='o', color='gray', size=2, alpha=0.5)
    + labs(x='Time (Day)', y='Y')
)
p.save(filename='output/fig11-14.py.png', dpi=300, width=6, height=5)
