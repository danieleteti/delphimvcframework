"""
Generates .ico files from Tabler Icons SVGs.
Requires: pip install Pillow cairosvg

Icons source: https://tabler.io/icons (MIT License)
"""

import cairosvg
from PIL import Image, ImageDraw
import io
import os

os.chdir(os.path.dirname(os.path.abspath(__file__)))

PRESETS = {
    'restful_api':  {'color': (66, 133, 244),   'svg': 'restful_api.svg'},
    'web_app':      {'color': (76, 175, 80),    'svg': 'web_app.svg'},
    'microservice': {'color': (156, 39, 176),   'svg': 'microservice.svg'},
    'realtime':     {'color': (255, 152, 0),    'svg': 'realtime.svg'},
    'fullstack':    {'color': (211, 47, 47),    'svg': 'fullstack.svg'},
    'custom':       {'color': (117, 117, 117),  'svg': 'custom.svg'},
}

BASE_SIZE = 256


def make_rounded_rect(size, color, radius):
    img = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)
    draw.rounded_rectangle(
        [(0, 0), (size - 1, size - 1)],
        radius=radius,
        fill=color + (255,),
    )
    return img


def load_svg_white(svg_file, size):
    png_data = cairosvg.svg2png(url=svg_file, output_width=size, output_height=size)
    img = Image.open(io.BytesIO(png_data)).convert('RGBA')
    pixels = img.load()
    for y in range(img.height):
        for x in range(img.width):
            r, g, b, a = pixels[x, y]
            if a > 0:
                pixels[x, y] = (255, 255, 255, a)
    return img


if __name__ == '__main__':
    for name, info in PRESETS.items():
        bg = make_rounded_rect(BASE_SIZE, info['color'], BASE_SIZE // 6)
        icon_size = int(BASE_SIZE * 0.6)
        svg_icon = load_svg_white(info['svg'], icon_size)
        offset = (BASE_SIZE - icon_size) // 2
        bg.paste(svg_icon, (offset, offset), svg_icon)
        ico_path = name + '.ico'
        bg.save(ico_path, format='ICO', sizes=[(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)])
        print(f'{ico_path}: {os.path.getsize(ico_path)} bytes')
    print('Done!')
