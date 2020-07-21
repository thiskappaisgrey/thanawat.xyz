const defaultTheme = require('tailwindcss/defaultTheme')
module.exports = {
  purge: ["./templates/*.html",
         "./*.html"],
  theme: {
    typography: {
      default: {
        css: {
          color: defaultTheme.colors.white,
          strong: {
            color: defaultTheme.colors.white,
            fontWeight: '600'
          },
          a: {
            color: defaultTheme.colors.blue[400],
            '&:hover': {
              color: defaultTheme.colors.blue[600],
            },
          },
          h1: {
            color: defaultTheme.colors.white,
            fontWeight: '800',
            marginTop: '1rem'
          },
          h2: {
            color: defaultTheme.colors.white,
            fontWeight: '700',
          },
          h3: {
            color: defaultTheme.colors.white,
            fontWeight: '600',
          },
          h4: {
            color: defaultTheme.colors.white,
            fontWeight: '600',
          },
          pre: {
            color: defaultTheme.colors.gray[700],
            fontFamily: defaultTheme.fontFamily.mono.join(', '),
            backgroundColor: defaultTheme.colors.white,
            overflowX: 'auto',
          },
          'pre code': {
              backgroundColor: 'transparent',
              borderWidth: '0',
              borderRadius: '0',
              padding: '0',
              fontWeight: '400',
              color: 'inherit',
              fontSize: 'inherit',
              fontFamily: 'inherit',
              lineHeight: 'inherit',
        },
          code: {
            color: defaultTheme.colors.gray[200],
            fontWeight: '600',
          }
        },
      },
    },
    extend: {},
  },
  variants: {},
  plugins: [
    require('@tailwindcss/typography')
  ],
}
