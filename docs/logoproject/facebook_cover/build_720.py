"""Da cover720.html al JPEG 720x376 che Facebook serve senza ridimensionare.
   chrome --headless --force-device-scale-factor=2 --window-size=1640,856 --screenshot=sim/master720.png cover720.html
   poi: python build_720.py"""
from PIL import Image, ImageFilter

master = Image.open('sim/master720.png').convert('RGB')
out = master.resize((720, 376), Image.LANCZOS).filter(
    ImageFilter.UnsharpMask(radius=0.8, percent=70, threshold=2))
out.save('dmvcframework_fbgroup_cover_720x376_optimized.jpg',
         quality=97, subsampling=0, optimize=True)
# anteprima di come lo stira il browser nello slot del gruppo
out.resize((1250, 653), Image.BICUBIC).save('sim/vista_membri_ottimizzata.png')
