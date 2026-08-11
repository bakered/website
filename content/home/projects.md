---
# An instance of the Portfolio widget.
# Documentation: https://wowchemy.com/docs/page-builder/
widget: portfolio

active: true
# This file represents a page section.
headless: true

# Order that this section appears on the page.
weight: 20

title: Apps & Projects
subtitle: ''

content:
  # Page type to display. E.g. project.
  page_type: project

  # Default filter index (e.g. 0 corresponds to the first `filter_button` instance below).
  filter_default: 0

  filter_button:
  - name: All
    tag: '*'
  - name: Live Apps
    tag: live-app
  - name: Data Viz
    tag: data-viz

design:
  # Choose how many columns the section has. Valid values: '1' or '2'.
  columns: '2'

  # Toggle between the various page layout types.
  #   1 = List
  #   2 = Compact
  #   3 = Card
  #   5 = Showcase
  view: 3

  # For Showcase view, flip alternate rows?
  flip_alt_rows: false
---
