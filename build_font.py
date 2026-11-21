"""Bake an OFL Noto CJK subset into a two-bit-coverage font for the Fortran renderer.

Only a compiler-time font file is required; users receive the generated glyph data.
"""
from pathlib import Path
import argparse
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--font', type=Path, default=ROOT / 'dependencies/NotoSansCJKsc-Regular.otf')
args = parser.parse_args()
labels = ('仪器时间分钟虚线表示设定程序模块温度计数开关周期循环秒最近阴影标示检测器采集荧光信号'
          '温控通道板孔独立传感主参考恢复正常停止缺失状态鼠标读取值图像上下左右' + '°·−（）；：')
labels += ''.join(chr(c) for c in __import__('json').loads((ROOT / 'font-codes.json').read_text()))
codes = list(range(32, 127)) + sorted({ord(c) for c in labels if ord(c) > 126})
font = ImageFont.truetype(str(args.font), 24)
rows, advances = [], []
for code in codes:
    image = Image.new('L', (28, 36)); draw = ImageDraw.Draw(image)
    draw.text((0, 28), chr(code), font=font, anchor='ls', fill=255)
    advances.append(max(3, min(28, round(font.getlength(chr(code))))))
    for y in range(36):
        value = sum(round(image.getpixel((x, y)) / 85) << (2 * x) for x in range(28))
        rows.append(value)

def array(name, values, kind):
    chunks = [values[i:i+6] for i in range(0, len(values), 6)]
    return f'  integer({kind}), parameter :: {name}({len(values)}) = [ &\n' + ', &\n'.join(
        '    ' + ','.join(str(x) + ('_c_int64_t' if kind == 'c_int64_t' else '') for x in chunk) for chunk in chunks) + ' ]\n'

source = ('! Generated from Noto Sans CJK SC (SIL OFL 1.1). See assets/licenses/noto-cjk.txt.\n'
          'module r2_font\n  use iso_c_binding, only: c_int,c_int64_t\n  implicit none\n'
          f'  integer, parameter :: font_count={len(codes)}, font_width=28, font_height=36\n'
          + array('glyph_codes', codes, 'c_int') + array('glyph_advance', advances, 'c_int')
          + ''.join(array('glyph_rows_'+str(i//768+1), rows[i:i+768], 'c_int64_t') for i in range(0,len(rows),768)) + '  integer(c_int64_t), parameter :: glyph_rows('+str(len(rows))+') = [ &\n'+', &\n'.join('    glyph_rows_'+str(i+1) for i in range((len(rows)+767)//768))+' ]\n' + 'end module\n')
(ROOT / 'fortran/r2_font.f90').write_bytes(source.encode())
print('Baked', len(codes), 'glyphs')
