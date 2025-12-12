import pytest
import requests
import time
import sys

BASE_URL = "http://localhost:8080"
HEADERS = {"Authorization": "Bearer super-secret"}

def wait_for_server():
    for _ in range(30):
        try:
            requests.get(BASE_URL, headers=HEADERS)
            return True
        except requests.exceptions.ConnectionError:
            time.sleep(1)
    return False

@pytest.fixture(scope="session", autouse=True)
def ensure_server_running():
    if not wait_for_server():
        pytest.fail("Server did not start in time")

def test_root():
    resp = requests.get(f"{BASE_URL}/", headers=HEADERS)
    assert resp.status_code == 200
    assert "SUCCESS! Kamyu is working!" in resp.text
    assert resp.headers.get("X-Powered-By") == "Kamyu"

def test_home():
    resp = requests.get(f"{BASE_URL}/home", headers=HEADERS)
    assert resp.status_code == 200
    assert "Home is here" in resp.text

def test_user_id():
    user_id = "123"
    resp = requests.get(f"{BASE_URL}/user/{user_id}", headers=HEADERS)
    assert resp.status_code == 200
    assert f"User ID: {user_id}" in resp.text

def test_search():
    resp = requests.get(f"{BASE_URL}/search", params={"q": "python", "page": 2}, headers=HEADERS)
    assert resp.status_code == 200
    assert "Search: python, page: 2" in resp.text

def test_create_person():
    payload = {"name": "Alice", "age": 30}
    resp = requests.post(f"{BASE_URL}/cities/London/people", json=payload, headers=HEADERS)
    assert resp.status_code == 201
    data = resp.json()
    assert data["fullName"] == "Alice from London"
    assert data["personAge"] == 30
    assert data["identifier"] == 1

def test_unauthorized():
    resp = requests.get(f"{BASE_URL}/", headers={})
    assert resp.status_code == 401
    assert "Missing or invalid token" in resp.text
