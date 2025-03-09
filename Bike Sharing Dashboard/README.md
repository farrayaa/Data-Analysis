# Bike Sharing Dashboard

An interactive dashboard for analyzing bike-sharing data using Streamlit.

![Dashboard Preview](images/Dashboard%20Bike%20Sharing.png)

## Features
- Visualization of bike rental data  
- Analysis of rental trends over time  
- Interactive filters and dynamic charts  

## Installation and Running the Application

### 1. Clone the Repository
```sh
git clone <repository_url>
cd bike-sharing-dashboard
```

### 2. Create and Activate a Virtual Environment (Windows)
```sh
python -m venv venv
venv\Scripts\activate
```

### 3. Install Dependencies
```sh
pip install -r requirements.txt
```

If `requirements.txt` is not available, install manually:
```sh
pip install streamlit pandas numpy matplotlib seaborn
pip freeze > requirements.txt
```

### 4. Run the Application
```sh
streamlit run app.py
```

## Folder Structure
```
.
├── app.py              # Main Streamlit script
├── data/               # Folder for storing dataset
├── images/             # Folder for storing images
│   ├── dashboard_preview.png
├── venv/               # Virtual environment
├── requirements.txt    # List of dependencies
├── README.md           # Documentation
```
